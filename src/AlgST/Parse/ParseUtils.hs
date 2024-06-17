{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module AlgST.Parse.ParseUtils
  ( -- * Parse monad
    Parser (..),
    runParser,
    scanToken,
    skippingNLs,
    LexFn (..),
    UnscopedName (..),
    scopeName,
    ParsedModule (..),
    emptyParsedModule,
    resolveImports,

    -- * Errors
    addError,
    addErrors,
    fatalError,

    -- ** Error messages
    errorNoTermLinLambda,
    errorRecBadRhsLambda,
    errorMultipleWildcards,
    errorMisplacedPairCon,
    errorDuplicateBind,
    errorInvalidKind,
    errorUnexpectedTokens,
    errorMissingOperand,
    errorUnknownPragma,

    -- * Operators
    Parenthesized (..),
    sectionsParenthesized,

    -- * Type declarations
    typeConstructors,

    -- * Assembling of modules
    ModuleBuilder,
    BuilderState,
    runModuleBuilder,
    liftModuleBuilder,
    moduleValueDecl,
    moduleValueBinding,
    moduleTypeDecl,
    insertBenchmark,

    -- ** Import statements
    ImportItem (..),
    mkImportItem,
    mergeImportAll,
    mergeImportOnly,

    -- * Checking for duplicates
    DuplicateError,
    insertNoDuplicates,
    mergeNoDuplicates,
  )
where

import AlgST.Parse.Lexer
import AlgST.Parse.LexerUtils
import AlgST.Parse.Phase
import AlgST.Parse.Token
import AlgST.Syntax.Decl
import AlgST.Syntax.Expression qualified as E
import AlgST.Syntax.Module
import AlgST.Syntax.Name
import AlgST.Syntax.Operators
import AlgST.Syntax.Tree qualified as T
import AlgST.Util.Diagnose qualified as D
import AlgST.Util.Lenses qualified as L
import AlgST.Util.Operators
import AlgST.Util.SourceManager
import Control.Arrow (Kleisli (..), (>>>))
import Control.Monad
import Control.Monad.Eta
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Validate
import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.DList.DNonEmpty qualified as DNE
import Data.Foldable
import Data.Function
import Data.Functor.Identity
import Data.HashMap.Strict qualified as HM
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Merge.Strict qualified as Merge
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Singletons
import Lens.Family2 hiding ((&))
import Lens.Family2.Stock (_1, _2)
import Numeric.Natural

data ParsedModule = ParsedModule
  { parsedImports :: [Located (Import ModuleName)],
    parsedModule :: PModule
  }

emptyParsedModule :: ParsedModule
emptyParsedModule = ParsedModule [] emptyModule

resolveImports ::
  (Applicative f) =>
  (ModuleName -> f a) ->
  ParsedModule ->
  f [Located (Import (ModuleName, a))]
resolveImports lookupModule = do
  let resolve name = (name,) <$> lookupModule name
  traverse (traverse (traverse resolve)) . parsedImports

instance T.LabeledTree ParsedModule where
  labeledTree pm =
    [ T.tree "imports" [T.labeledTree (parsedImports pm)],
      T.tree "declarations" [T.labeledTree (parsedModule pm)]
    ]

data ImportMergeState = IMS
  { -- | A subset of the keys of 'imsRenamed'.
    imsAsIs :: !(HashSet ImportKey),
    imsHidden :: !ImportHidden,
    imsRenamed :: !ImportRenamed
  }

emptyMergeState :: ImportMergeState
emptyMergeState = IMS mempty mempty mempty

{- ORMOLU_DISABLE -}
L.makeLenses ''ImportMergeState
imsAsIsL :: Lens' ImportMergeState (HashSet ImportKey)
imsHiddenL :: Lens' ImportMergeState ImportHidden
imsRenamedL :: Lens' ImportMergeState ImportRenamed
{- ORMOLU_ENABLE -}

newtype UnscopedName = UName (forall scope. PName scope)

scopeName :: UnscopedName -> PName scope
scopeName (UName n) = n

addError :: D.Diagnostic -> Parser ()
addError !d = dispute (pure d)

addErrors :: [D.Diagnostic] -> Parser ()
addErrors [] = pure ()
addErrors (e : es) = dispute $ DNE.fromNonEmpty $ e :| es

fatalError :: D.Diagnostic -> Parser a
fatalError !d = refute (pure d)

data Parenthesized
  = TopLevel
  | InParens !SrcRange
  deriving (Eq)

sectionsParenthesized :: (SingI h) => Parenthesized -> OperatorSequence h Parse PExp -> Parser PExp
sectionsParenthesized TopLevel ops = do
  traverse_ (addError . errorMissingOperand (Just (getRange ops))) (sectionOperator ops)
  pure $ E.Exp $ Right $ SomeOperatorSequence ops
sectionsParenthesized (InParens r) ops = do
  -- If we have a section we want to extend the sequence's range to include the
  -- parentheses.
  let extendRange = if isSection ops then id else rangeL %~ (<> r)
  pure $ E.Exp $ Right $ SomeOperatorSequence $ extendRange ops

errorMissingOperand :: (HasRange o) => Maybe SrcRange -> o -> D.Diagnostic
errorMissingOperand sectionRange op =
  D.err "missing operand"
    & D.primary (getRange op) "operator is missing an operand"
    & maybe id sectionFix sectionRange
  where
    sectionFix r = D.fix r "wrap it in parentheses for an operator section"
{-# NOINLINE errorMissingOperand #-}

typeConstructors ::
  TypeVar PStage ->
  TypeDecl Parse ->
  NameMap Values (ConstructorDecl Parse)
typeConstructors = declConstructors

data BuilderState = BuilderState
  { builderCurValueDecl :: !(Maybe (Located (PName Values), PType)),
    builderBenchmarkCount :: !Natural
  }

type ModuleBuilder =
  Kleisli (StateT BuilderState Parser) PModule PModule

runModuleBuilder :: ModuleBuilder -> Parser PModule
runModuleBuilder builder = evalStateT (buildModule emptyModule) builderState
  where
    Kleisli buildModule =
      builder >>> completePrevious >>> reverseBench
    reverseBench = Kleisli \p ->
      pure p {moduleBench = reverse (moduleBench p)}
    builderState =
      BuilderState
        { builderCurValueDecl = Nothing,
          builderBenchmarkCount = 0
        }

liftModuleBuilder :: Parser () -> ModuleBuilder
liftModuleBuilder m = Kleisli \p -> p <$ lift m

completePrevious :: ModuleBuilder
completePrevious = Kleisli \p -> do
  bst <- get
  case builderCurValueDecl bst of
    Nothing ->
      pure p
    Just (loc :@ name, sig) -> do
      put $! bst {builderCurValueDecl = Nothing}
      let decl = SignatureDecl loc sig
      sigs <- lift $ insertNoDuplicates name decl (moduleSigs p)
      pure p {moduleSigs = sigs}

insertBenchmark :: Benchmark Parse -> ModuleBuilder
insertBenchmark bench =
  completePrevious >>> Kleisli \p -> do
    bst <- get
    let n = builderBenchmarkCount bst + 1
    let namedBench
          | null (benchName bench) = bench {benchName = show n}
          | otherwise = bench
    put $! bst {builderBenchmarkCount = n}
    -- We accumulate the benchmarks in reverse. They will be reversed (to be in
    -- the correct order) by `runModuleBuilder`.
    pure p {moduleBench = namedBench : moduleBench p}

moduleValueDecl :: Located (ProgVar PStage) -> PType -> ModuleBuilder
moduleValueDecl valueName ty =
  completePrevious >>> Kleisli \p -> do
    modify' \bst -> bst {builderCurValueDecl = Just (valueName, ty)}
    pure p

moduleValueBinding :: Located (ProgVar PStage) -> [Located AName] -> PExp -> ModuleBuilder
moduleValueBinding valueName params e = Kleisli \p0 -> do
  mincomplete <- gets builderCurValueDecl
  p <-
    -- If there is an incomplete definition which does not match the current
    -- variable, we have to add it to the "imported" signatures.
    if
      | Just (prevName, _) <- mincomplete,
        onUnL (/=) valueName prevName ->
          runKleisli completePrevious p0
      | otherwise ->
          pure p0

  -- Re-read the incomplete binding, might be changed by the call to
  -- 'validateNotIncomplete' and remember that there is no incomplete binding
  -- now.
  mincomplete' <- gets builderCurValueDecl
  modify' \bst -> bst {builderCurValueDecl = Nothing}
  case mincomplete' of
    Nothing -> lift do
      addError $
        D.err "missing declaration"
          & D.primary (getRange valueName) "binding must be preceeded by its declaration"
      pure p
    Just (defLoc :@ _, ty) -> lift do
      let decl =
            ValueDecl
              { valuePos = defLoc,
                valueType = ty,
                valueParams = params,
                valueBody = e
              }
      parsedValues' <-
        insertNoDuplicates
          (unL valueName)
          (Right decl)
          (moduleValues p)
      when (unL valueName `Map.member` moduleSigs p) do
        addError $ errorImportShadowed (getRange valueName)
      pure p {moduleValues = parsedValues'}

moduleTypeDecl :: TypeVar PStage -> TypeDecl Parse -> ModuleBuilder
moduleTypeDecl v tydecl =
  completePrevious >>> Kleisli \p -> do
    parsedTypes' <- lift $ insertNoDuplicates v tydecl (moduleTypes p)
    let constructors = Left <$> typeConstructors v tydecl
    parsedValues' <- lift $ mergeNoDuplicates (moduleValues p) constructors
    pure p {moduleTypes = parsedTypes', moduleValues = parsedValues'}

data ImportItem = ImportItem
  { importScope :: !Scope,
    importIdent :: !Unqualified,
    importBehaviour :: !ImportBehaviour,
    importItemRange :: !SrcRange
  }

instance HasRange ImportItem where
  getRange = importItemRange

importKey :: ImportItem -> ImportKey
importKey = (,) <$> importScope <*> importIdent

mkImportItem ::
  (SrcRange -> Located Scope) ->
  Located Unqualified ->
  ImportBehaviour ->
  ImportItem
mkImportItem getScope ident behaviour =
  ImportItem
    { importScope = scope,
      importIdent = unL ident,
      importBehaviour = behaviour,
      importItemRange = range
    }
  where
    range :@ scope = getScope $ getRange ident

-- | Inserts the given 'ImportItem' into the 'ImportMergeState' or emits an
-- error message if the addition conflicts with imports already present.
--
-- ==== __@ImportItem@ Conflicts__
--
-- The table below describes which kinds of 'ImportItem's are compatible.
-- Renamed items are compared with other items based on the new name not the
-- original name.
--
-- +--------------+--------------+--------------+--------------+
-- |              | __hidden__   | __as-is__    | __renamed__  |
-- +--------------+--------------+--------------+--------------+
-- | __hidden__   | ✱            | ✗            | ✓            |
-- +--------------+--------------+--------------+--------------+
-- | __as-is__    | ✗            | ✱            | ✗            |
-- +--------------+--------------+--------------+--------------+
-- | __renamed__  | ✓            | ✗            | ✗            |
-- +--------------+--------------+--------------+--------------+
--
-- [✓]: Mixing an explicit hide with a rename to the hidden name is
-- __accepted:__
--
--     > import Some.Module (*, someName as _, otherName as someName)
--
-- [✗]: Mixing of as-is imports while hiding the same name, or mixing a rename
-- to /X/ with any other rename to /X/ is __disallowed:__
--
--     > import Some.Module (someName, someName as _)
--     > import Some.Module (someName, someOtherName as someName)
--     > import Some.Module (name1 as someName, name2 as someName)
--
-- [✱]: Importing the same identifier twice as-is, or hiding it twice is
-- accepted for now. Once we have the infrastructure for warnings we might want
-- to emit a warning.
--
--     > import Some.Module (someName, someName)
--     > import Some.Module (*, someName as _, someName as _)
addImportItem :: SrcRange -> ImportMergeState -> ImportItem -> Parser ImportMergeState
addImportItem importStmtRange ims ii@ImportItem {..} = case importBehaviour of
  ImportHide
    | Just other <- HM.lookup (importKey ii) (imsRenamed ims),
      HS.member (importKey ii) (imsAsIs ims) ->
        -- Hiding once and importing as-is conflicts.
        conflict $ other @- ImportAsIs
    | otherwise -> ok do
        -- Hiding twice is alright (we might want to emit a warning). Hiding also
        -- explicitly allows some other identifier to reuse the name.
        imsHiddenL . L.hashAt (importKey ii) .~ Just importItemRange
  ImportAsIs
    | Just hideLoc <- HM.lookup (importKey ii) (imsHidden ims) ->
        -- Hiding once and importing as-is conflicts.
        conflict $ hideLoc @- ImportHide
    | Just (otherLoc :@ orig) <- HM.lookup (importKey ii) (imsRenamed ims),
      not $ HS.member (importKey ii) (imsAsIs ims) ->
        -- Importing once as-is and mapping another identifier to this name
        -- conflicts.
        conflict $ otherLoc @- ImportFrom orig
    | otherwise -> ok do
        -- Importing twice as-is is alright (we might want to emit a warning).
        -- Remeber this import.
        imsAsIsL %~ HS.insert (importKey ii)
          >>> imsRenamedL . L.hashAt (importKey ii)
            .~ Just (importItemRange @- importIdent)
  ImportFrom orig
    | Just (otherLoc :@ otherName) <- HM.lookup (importKey ii) (imsRenamed ims) -> do
        -- Mapping another identifier to the same name conflicts, be it via an
        -- explicit rename or an as-is import.
        let isAsIs = HS.member (importKey ii) (imsAsIs ims)
        conflict $ otherLoc @- if isAsIs then ImportAsIs else ImportFrom otherName
    | otherwise -> ok do
        -- An explicit hide is ok.
        imsRenamedL . L.hashAt (importKey ii)
          .~ Just (importItemRange @- orig)
  where
    ok f = pure (f ims)
    conflict other =
      ims
        <$ addErrors
          [ errorConflictingImports
              importStmtRange
              (importKey ii)
              other
              (importItemRange @- importBehaviour)
          ]

mergeImportAll :: SrcRange -> SrcRange -> [ImportItem] -> Parser ImportSelection
mergeImportAll stmtRange allRange =
  foldM (addImportItem stmtRange) emptyMergeState
    >>> fmap \ims -> do
      -- Add the implicitly hidden set to the explicitly hidden set. If an
      -- identifier is hidden explicitly we prefer that entry.
      --
      -- At the moment implicit hides are associated with 'ZeroPos'.
      -- TODO: When we have error messages of the form “identifier was
      -- (implicitly) hidden at …” we might want to keep the location of the
      -- implicit hide including a differntiation between implicit and explicit
      -- hides.
      let allHidden =
            HM.foldlWithKey'
              (\hm (scope, _) (_ :@ u) -> HM.insertWith const (scope, u) NullRange hm)
              (imsHidden ims)
              (imsRenamed ims)
      ImportAll allRange allHidden (imsRenamed ims)

mergeImportOnly :: SrcRange -> [ImportItem] -> Parser ImportSelection
mergeImportOnly stmtRange =
  foldM (addImportItem stmtRange) emptyMergeState
    >>> fmap (ImportOnly . imsRenamed)

-- | Inserts the value under the given key into the map. If there is already a
-- value under that key an error as with 'errorMultipleDeclarations' is added
-- and the value is not changed.
insertNoDuplicates ::
  (DuplicateError k v, Ord k) => k -> v -> Map.Map k v -> Parser (Map.Map k v)
insertNoDuplicates k v m = mergeNoDuplicates m $ Map.singleton k v

-- | Merges two maps, for any overlapping keys an error as with
-- 'errorMultipleDeclarations' is added.
--
-- In case of any merge duplicates the unmerged map will be returned.
mergeNoDuplicates ::
  (DuplicateError k v, Ord k) => Map.Map k v -> Map.Map k v -> Parser (Map.Map k v)
mergeNoDuplicates m new =
  merge m new
    & tolerate
    & fmap (fromMaybe m)
  where
    merge =
      Merge.mergeA
        Merge.preserveMissing
        Merge.preserveMissing
        (Merge.zipWithAMatched dupError)
    dupError k v1 v2 =
      fatalError $ duplicateError k v1 v2

class DuplicateError k a where
  duplicateError :: k -> a -> a -> D.Diagnostic

-- | Message for duplicate type declarations.
instance DuplicateError (Name PStage Types) (TypeDecl Parse) where
  duplicateError _ = errorMultipleDeclarations

-- | Message for a duplicated top-level value declaration. This includes both
-- constrcutor names between two declarations, and top-level functions.
instance
  DuplicateError
    (Name PStage Values)
    (Either (ConstructorDecl Parse) (ValueDecl Parse))
  where
  duplicateError _ = errorMultipleDeclarations

instance DuplicateError (Name PStage Values) (SignatureDecl Parse) where
  duplicateError _ = errorMultipleDeclarations

-- | Message for a duplicated constructor inside a type declaration.
instance DuplicateError (Name PStage Values) (SrcRange, [PType]) where
  duplicateError _ (r1, _) (r2, _) = errorMultipleDeclarations r1 r2

-- | Message for a duplicate branch in a case expression:
--
-- > case … of { A -> …, A -> … }
instance DuplicateError (Name PStage Values) (E.CaseBranch f Parse) where
  duplicateError _ x y = errorDuplicateBranch (getRange x) (getRange y)

-- | Messages for any form of duplicate binding:
--
-- * patterns
-- * lambda abstractions (not yet implemented)
-- * type parameters
-- * top-level function parameters
instance DuplicateError a SrcRange where
  duplicateError _ = errorDuplicateBind

errorMultipleDeclarations :: (HasRange a1, HasRange a2) => a1 -> a2 -> D.Diagnostic
errorMultipleDeclarations (getRange -> r1) (getRange -> r2) =
  D.err "duplicate declaration"
    & D.primary (min r1 r2) "first declaration"
    & D.primary (max r1 r2) "second declaration"
{-# NOINLINE errorMultipleDeclarations #-}

errorDuplicateBind :: (HasRange a1, HasRange a2) => a1 -> a2 -> D.Diagnostic
errorDuplicateBind (getRange -> r1) (getRange -> r2) =
  D.err "duplicate binding"
    & D.primary (min r1 r2) "first binding"
    & D.primary (max r1 r2) "second binding"
{-# NOINLINE errorDuplicateBind #-}

errorDuplicateBranch :: (HasRange a1, HasRange a2) => a1 -> a2 -> D.Diagnostic
errorDuplicateBranch (getRange -> r1) (getRange -> r2) =
  D.err "duplicate case alternative"
    & D.primary (min r1 r2) "first occurence"
    & D.primary (max r1 r2) "second occurence"
{-# NOINLINE errorDuplicateBranch #-}

-- TODO: Is this error message still up-to date? This "import/builtin shadowed"
-- stuff feels outdated now that we have the module system.
errorImportShadowed :: SrcRange -> D.Diagnostic
errorImportShadowed range =
  D.err "import/builtin shadowed"
    & D.primary range "declaration shadows import/builtin of the same name"
{-# NOINLINE errorImportShadowed #-}

-- | An warning message for when a lambda binds only type variables but uses
-- the linear arrow @-o@. This combination does not make sense, therefore we do
-- not allow it.
errorNoTermLinLambda :: SrcRange -> SrcRange -> D.Diagnostic
errorNoTermLinLambda absRange arrRange =
  D.warn "unnecessary linear arrow"
    & D.fix arrRange ("prefer an unrestricted arrow" <+> D.syntax "->")
    & D.context absRange "lambda abstraction binds only type variables"
{-# NOINLINE errorNoTermLinLambda #-}

errorRecBadRhsLambda :: SrcRange -> SrcRange -> PExp -> D.Diagnostic
errorRecBadRhsLambda fullExpr _recToken recExpr =
  D.err "invalid ‘rec’ expression"
    & D.primary (getRange recExpr) msg
    & D.primary fullExpr ("this" <+> srec <+> "expression")
    & D.hint hint
  where
    (msg, hint) = case recExpr of
      E.TypeAbs {} ->
        ( msgPrefix <+> "binds only type variables",
          hintPrefix <+> "bind at least one term variable."
        )
      _ ->
        ( msgPrefix <+> "is not a lambda abstraction",
          hintPrefix <+> "consist of a lambda abstraction binding a term variable."
        )
    srec = D.syntax "rec"
    msgPrefix = srec <+> "right-hand side"
    hintPrefix = "a" <+> srec <+> "expression's right-hand side must"
{-# NOINLINE errorRecBadRhsLambda #-}

errorMultipleWildcards ::
  E.CaseBranch Identity Parse -> E.CaseBranch Identity Parse -> D.Diagnostic
errorMultipleWildcards (getRange -> x) (getRange -> y) =
  D.err "multiple wildcard branches in case expression"
    & D.primary (min x y) "first occurence"
    & D.context (max x y) "second occurence"
{-# NOINLINE errorMultipleWildcards #-}

errorMisplacedPairCon :: forall (s :: Scope). (SingI s) => SrcRange -> D.Diagnostic
errorMisplacedPairCon loc =
  D.err "misplaced pair constructor"
    & D.primary loc msg
    & D.hint "Use the ‘(…, …)’ form when outside of patterns and ‘select’"
  where
    msg =
      eitherName @s @D.Doc
        "cannot appear as a type constructor"
        "cannot appear as an expression"
{-# NOINLINE errorMisplacedPairCon #-}

errorConflictingImports ::
  SrcRange ->
  ImportKey ->
  Located ImportBehaviour ->
  Located ImportBehaviour ->
  D.Diagnostic
errorConflictingImports importLoc (_scope, _name) i1 i2 =
  D.err "conflicting imports"
    & describe i1
    & describe i2
    & D.context importLoc "import is here"
  where
    describe (r :@ i) = D.primary r case i of
      ImportHide -> "hidden here"
      ImportAsIs -> "imported here"
      ImportFrom _ -> "imported as a renaming here"
{-# NOINLINE errorConflictingImports #-}

errorInvalidKind :: SrcRange -> D.Diagnostic
errorInvalidKind r =
  D.err "invalid kind"
    & D.primary r "not one of the valid kinds"
{-# NOINLINE errorInvalidKind #-}

errorUnexpectedTokens :: Token -> Parser a
errorUnexpectedTokens t = fatalError do
  let description = case t of
        TokenEof {} -> "unexpected end of file"
        _ -> "unexpected token"
  D.err "parse error"
    & D.primary (getRange t) description
{-# NOINLINE errorUnexpectedTokens #-}

skippingNLs :: Parser a -> Parser a
skippingNLs (Parser m) = Parser do
  local (_1 .~ NLSkipAll) m

errorUnknownPragma :: SrcRange -> SrcRange -> D.Diagnostic
errorUnknownPragma _pragmaRange keywordRange =
  D.err "invalid pragma"
    & D.fix keywordRange "unknown pragma kind, try ‘BENCHMARK’ or ‘BENCHMARK!’"

scanToken :: (Token -> Parser a) -> Parser a
scanToken k = Parser ask >>= \(nlp, lf) -> runLexFn (lf nlp) k

-- | Calls into the lexer to retrieve the next token. It updates the field
-- 'parseNextToken' for the next token.
--
-- TODO: Can we use lexer states to simplify this abomination?
scanTokenImpl :: ByteString -> NewlinePolicy -> LexFn
scanTokenImpl input nlp = LexFn \k -> case alexScan input 0 of
  AlexEOF -> Parser $ etaReaderT do
    -- We will always return EOF now.
    let eof = TokenEof (fullRange input)
    let onlyEof = LexFn \k -> k eof
    local (_2 .~ const onlyEof) do
      unParser (k eof)
  AlexError _ -> do
    -- This branch can only happen with invalid UTF8. Any unexpected
    -- (but correctly encoded) characters are caught by the lexer.
    input' <- invalidUTF8 input
    runLexFn (scanTokenImpl input' nlp) k
  AlexSkip input' _ -> do
    -- Just continue with the remaining part.
    runLexFn (scanTokenImpl input' nlp) k
  AlexToken input' len act -> do
    -- Extract the matched part.
    let s = BS.take len input
    -- Run the action.
    case act s of
      -- In case of a diagnostic, add it to our list of diagnostics.
      -- TODO: Should this transition the newline policy?
      Left d -> dispute (pure d) *> runLexFn (scanTokenImpl input' nlp) k
      Right nl@(TokenNL _) ->
        case nlp of
          -- Lexing a NL in skipNL mode just advances to the next token.
          NLSkipAll -> runLexFn (scanTokenImpl input' nlp) k
          NLSkipFollowing -> runLexFn (scanTokenImpl input' nlp) k
          -- Otherwise we have to continue looking for the next token (now
          -- skipping NL!) in order to skip any trailing NL tokens.
          NLSkipConsecutive -> runLexFn (scanTokenImpl input' NLSkipFollowing) \case
            -- If the next token is EOF, we emit EOF immediately.
            eof@TokenEof {} -> k eof
            -- Otherwise, we emit our newline token.
            next -> Parser do
              -- Adjust the current lexing function, which would lex the token
              -- _after_ `next`, to emit `next` first.
              let prefix :: (NewlinePolicy -> LexFn) -> (NewlinePolicy -> LexFn)
                  prefix lexFn _nlp = LexFn \k -> Parser do
                    -- Once we have `next` emitted, we continue on with the
                    -- `LexFn` installed by the surroinding lex function.
                    local (_2 .~ lexFn) do
                      unParser (k next)
              local (_2 %~ prefix) do
                unParser (k nl)
      -- Otherwise we have found a valid token.
      Right t -> Parser do
        -- Set the lexing function accordingly. The current token isn't a
        -- newline so we don't start to skip the next newline tokens.
        let !nlp' = policyAfterToken nlp
        local (const (nlp', scanTokenImpl input')) do
          -- Call the continuation with the resulting token.
          unParser (k t)

runParser :: Parser a -> ByteString -> Either D.Errors a
runParser p input =
  unParser p
    & flip runReaderT (NLSkipFollowing, scanTokenImpl input)
    & runValidate
    & first DNE.toNonEmpty
