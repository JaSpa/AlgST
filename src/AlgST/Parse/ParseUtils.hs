{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module AlgST.Parse.ParseUtils
  ( -- * Lexer Utilities

    -- ** Alex definitions
    AlexInput,
    alexGetByte,
    alexInputPrevChar,

    -- ** Lexer actions
    LexAction,
    simpleToken,
    textToken,
    textToken',

    -- ** Errors
    invalidChar,
    invalidUTF8,

    -- * Parse monad
    ParseM,
    runParseM,
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
    errorRecNoTermLambda,
    errorMultipleWildcards,
    errorMisplacedPairCon,
    errorDuplicateBind,
    errorInvalidKind,
    errorUnexpectedTokens,
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

import AlgST.Parse.Phase
import AlgST.Parse.Token
import AlgST.Syntax.Decl
import AlgST.Syntax.Expression qualified as E
import AlgST.Syntax.Module
import AlgST.Syntax.Name
import AlgST.Syntax.Operators
import AlgST.Syntax.Pos qualified as P
import AlgST.Syntax.Tree qualified as T
import AlgST.Util.Diagnose (DErrors)
import AlgST.Util.Diagnose qualified as D
import AlgST.Util.Lenses qualified as L
import AlgST.Util.SourceManager
import Control.Arrow
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Validate
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.DList.DNonEmpty qualified as DNE
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
import Data.Word
import GHC.Foreign qualified as GHC
import Lens.Family2 hiding ((&))
import Numeric (showHex)
import Numeric.Natural
import System.IO qualified as IO
import System.IO.Unsafe

type LexAction = ByteString -> Either D.Diagnostic Token

type AlexInput = ByteString

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte = BS.uncons

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = error "left context not implemented"
{-# WARNING alexInputPrevChar "left context not implemented" #-}

simpleToken :: (SrcRange -> Token) -> LexAction
simpleToken f = Right . f . fullRange

textToken :: (Located String -> Token) -> LexAction
textToken = textToken' id

textToken' :: (String -> a) -> (Located a -> Token) -> LexAction
textToken' f g s = Right $ g $ fullRange s @- f decoded
  where
    decoded = unsafeDupablePerformIO do
      BS.useAsCStringLen s (GHC.peekCStringLen IO.utf8)

invalidChar :: LexAction
invalidChar s = Left do
  D.err
    (fullRange s)
    "invalid source character"
    "skipping this character, trying to continue"

-- | Emits an error about invalid UTF-8. We try to recover and return the
-- remaining input.
invalidUTF8 :: (MonadValidate D.DErrors m) => AlexInput -> m AlexInput
invalidUTF8 s =
  -- TODO: Implement recovery.
  refute (pure err)
  where
    err =
      D.err
        (SizedRange (startLoc s) 1)
        "invalid UTF-8"
        ("unexpected byte 0x" ++ showByte (BS.head s))
        & D.hint "I stopped reading the input file here."
    showByte b =
      "0x" ++ case showHex b "" of
        hex@[_] -> '0' : hex
        hex -> hex

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

type ParseM = ReaderT Buffer (Validate DErrors)

-- | Evaluates a value in the 'ParseM' monad producing a list of errors.
runParseM :: Buffer -> ParseM a -> Either D.Errors a
runParseM buf =
  flip runReaderT buf
    >>> mapErrors DNE.toNonEmpty
    >>> runValidate

newtype UnscopedName = UName (forall scope. PName scope)

scopeName :: UnscopedName -> PName scope
scopeName (UName n) = n

addError :: D.Diagnostic -> ParseM ()
addError !d = dispute (pure d)

addErrors :: [D.Diagnostic] -> ParseM ()
addErrors [] = pure ()
addErrors (e : es) = dispute $ DNE.fromNonEmpty $ e :| es

fatalError :: D.Diagnostic -> ParseM a
fatalError !d = refute (pure d)

data Parenthesized
  = TopLevel
  | InParens
  deriving (Eq)

sectionsParenthesized :: Parenthesized -> OperatorSequence Parse -> ParseM PExp
sectionsParenthesized TopLevel ops | Just op <- sectionOperator ops = do
  addError $
    D.err (needRange op) "missing argument" "operator is missing an argument"
      -- The range for the fix should include operator + the one argument we have
      & D.fix (needRange op) "wrap it in parentheses for an operator section"
  pure $ E.Exp $ Right ops
sectionsParenthesized _ ops = do
  pure $ E.Exp $ Right ops

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
  Kleisli (StateT BuilderState ParseM) PModule PModule

runModuleBuilder :: ModuleBuilder -> ParseM PModule
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

liftModuleBuilder :: ParseM () -> ModuleBuilder
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
        D.err
          (needRange valueName)
          "missing declaration"
          "binding should be preceeded by its declaration"
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
        addError $ errorImportShadowed (needRange valueName)
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
addImportItem :: SrcRange -> ImportMergeState -> ImportItem -> ParseM ImportMergeState
addImportItem importStmtRange ims ii@ImportItem {..} = case importBehaviour of
  ImportHide
    | Just other <- HM.lookup (importKey ii) (imsRenamed ims),
      HS.member (importKey ii) (imsAsIs ims) ->
        -- Hiding once and importing as-is conflicts.
        conflict $ needRange other @- ImportAsIs
    | otherwise -> ok do
        -- Hiding twice is alright (we might want to emit a warning). Hiding also
        -- explicitly allows some other identifier to reuse the name.
        imsHiddenL . L.hashAt (importKey ii) .~ Just (needPos importItemRange)
  ImportAsIs
    | Just hideLoc <- HM.lookup (importKey ii) (imsHidden ims) ->
        -- Hiding once and importing as-is conflicts.
        conflict $ needRange hideLoc @- ImportHide
    | Just (otherLoc P.:@ orig) <- HM.lookup (importKey ii) (imsRenamed ims),
      not $ HS.member (importKey ii) (imsAsIs ims) ->
        -- Importing once as-is and mapping another identifier to this name
        -- conflicts.
        conflict $ needRange otherLoc @- ImportFrom orig
    | otherwise -> ok do
        -- Importing twice as-is is alright (we might want to emit a warning).
        -- Remeber this import.
        imsAsIsL %~ HS.insert (importKey ii)
          >>> imsRenamedL . L.hashAt (importKey ii)
            .~ Just (needPos importItemRange P.@- importIdent)
  ImportFrom orig
    | Just (otherLoc P.:@ otherName) <- HM.lookup (importKey ii) (imsRenamed ims) -> do
        -- Mapping another identifier to the same name conflicts, be it via an
        -- explicit rename or an as-is import.
        let isAsIs = HS.member (importKey ii) (imsAsIs ims)
        conflict $ needRange otherLoc @- if isAsIs then ImportAsIs else ImportFrom otherName
    | otherwise -> ok do
        -- An explicit hide is ok.
        imsRenamedL . L.hashAt (importKey ii)
          .~ Just (needPos importItemRange P.@- orig)
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

mergeImportAll :: SrcRange -> SrcRange -> [ImportItem] -> ParseM ImportSelection
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
              (\hm (scope, _) (_ P.:@ u) -> HM.insertWith const (scope, u) P.ZeroPos hm)
              (imsHidden ims)
              (imsRenamed ims)
      ImportAll (needPos allRange) allHidden (imsRenamed ims)

mergeImportOnly :: SrcRange -> [ImportItem] -> ParseM ImportSelection
mergeImportOnly stmtRange =
  foldM (addImportItem stmtRange) emptyMergeState
    >>> fmap (ImportOnly . imsRenamed)

-- | Inserts the value under the given key into the map. If there is already a
-- value under that key an error as with 'errorMultipleDeclarations' is added
-- and the value is not changed.
insertNoDuplicates ::
  (DuplicateError k v, Ord k) => k -> v -> Map.Map k v -> ParseM (Map.Map k v)
insertNoDuplicates k v m = mergeNoDuplicates m $ Map.singleton k v

-- | Merges two maps, for any overlapping keys an error as with
-- 'errorMultipleDeclarations' is added.
--
-- In case of any merge duplicates the unmerged map will be returned.
mergeNoDuplicates ::
  (DuplicateError k v, Ord k) => Map.Map k v -> Map.Map k v -> ParseM (Map.Map k v)
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
  duplicateError _ x y = errorMultipleDeclarations (needRange x) (needRange y)

-- | Message for a duplicated top-level value declaration. This includes both
-- constrcutor names between two declarations, and top-level functions.
instance
  DuplicateError
    (Name PStage Values)
    (Either (ConstructorDecl Parse) (ValueDecl Parse))
  where
  duplicateError _ x y = errorMultipleDeclarations (needRange x) (needRange y)

instance DuplicateError (Name PStage Values) (SignatureDecl Parse) where
  duplicateError _ x y = errorMultipleDeclarations (needRange x) (needRange y)

-- | Message for a duplicated constructor inside a type declaration.
instance DuplicateError (Name PStage Values) (SrcRange, [PType]) where
  duplicateError _ (p1, _) (p2, _) = errorMultipleDeclarations (needRange p1) (needRange p2)

-- | Message for a duplicate branch in a case expression:
--
-- > case … of { A -> …, A -> … }
instance DuplicateError (Name PStage Values) (E.CaseBranch f Parse) where
  duplicateError _ x y = errorDuplicateBranch (needRange x) (needRange y)

-- | Messages for any form of duplicate binding:
--
-- * patterns
-- * lambda abstractions (not yet implemented)
-- * type parameters
-- * top-level function parameters
instance DuplicateError a SrcRange where
  duplicateError _ x y = errorDuplicateBind (needRange x) (needRange y)

errorMultipleDeclarations :: (HasRange a1, HasRange a2) => a1 -> a2 -> D.Diagnostic
errorMultipleDeclarations (getRange -> r1) (getRange -> r2) =
  D.err
    (max r1 r2)
    "duplicate declaration"
    "name is already defined in this module"
    & D.context (min r1 r2) "previous declaration"

errorDuplicateBind :: (HasRange a1, HasRange a2) => a1 -> a2 -> D.Diagnostic
errorDuplicateBind (getRange -> r1) (getRange -> r2) =
  D.err
    (max r1 r2)
    "duplicate binding"
    "binding conflicts with previous binding of the same name"
    & D.context (min r1 r2) "previous binding"
{-# NOINLINE errorDuplicateBind #-}

errorDuplicateBranch :: (HasRange a1, HasRange a2) => a1 -> a2 -> D.Diagnostic
errorDuplicateBranch (getRange -> r1) (getRange -> r2) =
  D.err
    (max r1 r2)
    "duplicate case alternative"
    "case alternative already provided"
    & D.context (min r1 r2) "previous location"

-- TODO: Is this error message still up-to date? This "import/builtin shadowed"
-- stuff feels outdated now that we have the module system.
errorImportShadowed :: SrcRange -> D.Diagnostic
errorImportShadowed range =
  D.err
    range
    "import/builtin shadowed"
    "declaration shadows import/builtin of the same name"
{-# NOINLINE errorImportShadowed #-}

-- | An warning message for when a lambda binds only type variables but uses
-- the linear arrow @-o@. This combination does not make sense, therefore we do
-- not allow it.
errorNoTermLinLambda :: SrcRange -> SrcRange -> D.Diagnostic
errorNoTermLinLambda absRange arrRange =
  D.warn arrRange "unnecessary linear arrow" "linear arrow does not make sense"
    & D.context absRange "lambda abstraction binds only type variables"
    & D.hint "Use an unrestricted arrow for this case."
{-# NOINLINE errorNoTermLinLambda #-}

errorRecNoTermLambda :: SrcRange -> SrcRange -> D.Diagnostic
errorRecNoTermLambda recLoc recExpr =
  D.err recExpr "invalid ‘rec’ expression" "invalid ‘rec’ right-hand side expression"
    & D.context recLoc "‘rec’ expression started here"
    & D.note note1
    & D.note note2
  where
    note1 =
      "a ‘rec’ expression's right-hand side must consist of a lambda \
      \abstraction."
    note2 =
      "a ‘rec’ expression must bind at least one non-type parameter in \
      \their right-hand side lambda abstraction."
{-# NOINLINE errorRecNoTermLambda #-}

errorMultipleWildcards ::
  E.CaseBranch Identity Parse -> E.CaseBranch Identity Parse -> D.Diagnostic
errorMultipleWildcards (needRange -> x) (needRange -> y) =
  D.err
    (min x y)
    "multiple wildcard branches in case expression"
    "here is the first wildcard branch"
    & D.context (max x y) "here is the second wildcard branch"

errorMisplacedPairCon :: forall (s :: Scope). (SingI s) => SrcRange -> D.Diagnostic
errorMisplacedPairCon loc =
  D.err loc "misplaced pair constructor" msg
    & D.hint "Use the ‘(…, …)’ form when outside of patterns and ‘select’"
  where
    msg =
      eitherName @s @String
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
  D.err importLoc "conflicting imports" "conflicting imports"
    & describe i1
    & describe i2
  where
    describe (r :@ i) = D.context r case i of
      ImportHide -> "hidden here"
      ImportAsIs -> "imported here"
      ImportFrom _ -> "imported as a renaming here"
{-# NOINLINE errorConflictingImports #-}

errorInvalidKind :: SrcRange -> D.Diagnostic
errorInvalidKind r = D.err r "invalid kind" "not one of the valid kinds"
{-# NOINLINE errorInvalidKind #-}

errorUnexpectedTokens :: [Token] -> ParseM a
errorUnexpectedTokens [] = do
  -- Generate an empty range at the very end of the file.
  end <- asks getEndLoc
  fatalError $ D.err (SrcRange end end) "parse error" "unexpected end of file"
errorUnexpectedTokens (t : _) = do
  fatalError $ D.err (getRange t) "parse error" "unexpected token"

errorUnknownPragma :: Token -> Token -> SrcRange -> D.Diagnostic
errorUnknownPragma pragmaStart pragmaEnd keywordRange =
  D.err pragmaRange "invalid pragma" "unknown pragma directive"
    & D.fix keywordRange "try ‘BENCHMARK’ or ‘BENCHMARK!’"
  where
    pragmaRange = SrcRange (getStartLoc pragmaStart) (getEndLoc pragmaEnd)
