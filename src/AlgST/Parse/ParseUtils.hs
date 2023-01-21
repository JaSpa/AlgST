{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
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
  ( -- * Parse monad
    ParseM,
    runParseM,
    UnscopedName (..),
    scopeName,
    ParsedModule (..),
    emptyParsedModule,
    resolveImports,
    partitionImports,

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

    -- * Operators
    Parenthesized (..),
    sectionsParenthesized,

    -- * Type declarations
    typeConstructors,

    -- * Assembling of modules
    ModuleBuilder,
    BuilderState,
    runModuleBuilder,
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
import AlgST.Syntax.Decl
import AlgST.Syntax.Expression qualified as E
import AlgST.Syntax.Module
import AlgST.Syntax.Name
import AlgST.Syntax.Operators
import AlgST.Syntax.Pos
import AlgST.Syntax.Tree qualified as T
import AlgST.Util.ErrorMessage
import AlgST.Util.Lenses qualified as L
import AlgST.Util.SourceLocation (SrcRange)
import AlgST.Util.SourceLocation qualified as R
import Control.Arrow
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Validate
import Data.DList.DNonEmpty (DNonEmpty)
import Data.DList.DNonEmpty qualified as DL
import Data.Either
import Data.Function
import Data.Functor.Identity
import Data.HashMap.Strict qualified as HM
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Merge.Strict qualified as Merge
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Singletons
import Lens.Family2 hiding ((&))
import Numeric.Natural

data ParsedModule = ParsedModule
  { parsedImports :: [Located (Import ModuleName)],
    parsedModule :: PModule
  }

emptyParsedModule :: ParsedModule
emptyParsedModule = ParsedModule [] emptyModule

resolveImports ::
  Applicative f =>
  (ModuleName -> f a) ->
  ParsedModule ->
  f [Located (Import (ModuleName, a))]
resolveImports lookupModule = do
  let resolve name = (name,) <$> lookupModule name
  traverse (traverse (traverse resolve)) . parsedImports

partitionImports ::
  (ModuleName -> Maybe a) ->
  ParsedModule ->
  (Map.Map ModuleName Pos, [Located (Import (ModuleName, a))])
partitionImports f =
  parsedImports
    >>> fmap (\locImp -> maybe (failed locImp) Right (resolve locImp))
    >>> partitionEithers
    >>> first (Map.fromListWith min)
  where
    failed (p :@ i) =
      Left (importTarget i, p)
    resolve =
      traverse (traverse (\n -> (n,) <$> f n))

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

type ParseM = Validate (DNonEmpty Diagnostic)

-- | Evaluates a value in the 'ParseM' monad producing a list of errors and
-- maybe a result.
runParseM :: ParseM a -> Either (NonEmpty Diagnostic) a
runParseM = mapErrors DL.toNonEmpty >>> runValidate

newtype UnscopedName = UName (forall scope. PName scope)

scopeName :: UnscopedName -> PName scope
scopeName (UName n) = n

addError :: Pos -> [ErrorMessage] -> ParseM ()
addError !p err = addErrors [PosError p err]

addErrors :: [Diagnostic] -> ParseM ()
addErrors [] = pure ()
addErrors (e : es) = dispute $ DL.fromNonEmpty $ e :| es

fatalError :: Diagnostic -> ParseM a
fatalError = refute . DL.singleton

data Parenthesized
  = TopLevel
  | InParens
  deriving (Eq)

sectionsParenthesized :: Parenthesized -> OperatorSequence Parse -> ParseM PExp
sectionsParenthesized TopLevel ops | Just op <- sectionOperator ops = do
  addError
    (pos op)
    [ Error "Operator",
      Error op,
      Error "is missing an argument.",
      ErrLine,
      Error "Wrap it in parentheses for an operator section."
    ]
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
      addError
        (pos valueName)
        [ Error "Binding of",
          Error valueName,
          Error "should be preceeded by its declaration."
        ]
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
        addErrors [uncurryL errorImportShadowed valueName]
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
    importLocation :: !Pos
  }

importKey :: ImportItem -> ImportKey
importKey = (,) <$> importScope <*> importIdent

mkImportItem ::
  (SrcRange -> Located Scope) ->
  R.Located Unqualified ->
  ImportBehaviour ->
  ImportItem
mkImportItem getScope ident behaviour =
  ImportItem
    { importScope = scope,
      importIdent = R.unL ident,
      importBehaviour = behaviour,
      importLocation = loc
    }
  where
    loc :@ scope = getScope $ R.getRange ident

-- | Inserts the given 'ImportItem' into the 'ImportMergeState' or emits an
-- error message if the addition conflicts with imports already present.
--
-- ==== __@ImportItem@ Conflicts__
--
-- The table below describes which kinds of 'ImportItem's are compatible.
-- Renamed items are are compared with other items based on the new name not
-- the original name.
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
addImportItem :: Pos -> ImportMergeState -> ImportItem -> ParseM ImportMergeState
addImportItem stmtLoc ims ii@ImportItem {..} = case importBehaviour of
  ImportHide
    | Just other <- HM.lookup (importKey ii) (imsRenamed ims),
      HS.member (importKey ii) (imsAsIs ims) ->
        -- Hiding once and importing as-is conflicts.
        conflict $ other @- ImportAsIs
    | otherwise ->
        -- Hiding twice is alright (we might want to emit a warning). Hiding also
        -- explicitly allows some other identifier to reuse the name.
        ok $ imsHiddenL . L.hashAt (importKey ii) .~ Just importLocation
  ImportAsIs
    | Just hideLoc <- HM.lookup (importKey ii) (imsHidden ims) ->
        -- Hiding once and importing as-is conflicts.
        conflict $ hideLoc @- ImportHide
    | Just (otherLoc :@ orig) <- HM.lookup (importKey ii) (imsRenamed ims),
      not $ HS.member (importKey ii) (imsAsIs ims) ->
        -- Importing once as-is and mapping another identifier to this name
        -- conflicts.
        conflict $ otherLoc @- ImportFrom orig
    | otherwise ->
        -- Importing twice as-is is alright (we might want to emit a warning).
        -- Remeber this import.
        ok $
          imsAsIsL %~ HS.insert (importKey ii)
            >>> imsRenamedL . L.hashAt (importKey ii) .~ Just (importLocation @- importIdent)
  ImportFrom orig
    | Just (otherLoc :@ otherName) <- HM.lookup (importKey ii) (imsRenamed ims) -> do
        -- Mapping another identifier to the same name conflicts, be it via an
        -- explicit rename or an as-is import.
        let isAsIs = HS.member (importKey ii) (imsAsIs ims)
        conflict $ otherLoc @- if isAsIs then ImportAsIs else ImportFrom otherName
    | otherwise ->
        -- An explicit hide is ok.
        ok $ imsRenamedL . L.hashAt (importKey ii) .~ Just (importLocation @- orig)
  where
    ok f = pure (f ims)
    conflict other =
      ims
        <$ addErrors
          [ errorConflictingImports
              stmtLoc
              (importKey ii)
              other
              (importLocation @- importBehaviour)
          ]

mergeImportAll :: Pos -> Pos -> [ImportItem] -> ParseM ImportSelection
mergeImportAll stmtLoc allLoc =
  foldM (addImportItem stmtLoc) emptyMergeState
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
              (\hm (scope, _) (_ :@ u) -> HM.insertWith const (scope, u) ZeroPos hm)
              (imsHidden ims)
              (imsRenamed ims)
      ImportAll allLoc allHidden (imsRenamed ims)

mergeImportOnly :: Pos -> [ImportItem] -> ParseM ImportSelection
mergeImportOnly stmtLoc =
  foldM (addImportItem stmtLoc) emptyMergeState
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
  duplicateError :: k -> a -> a -> Diagnostic

-- | Message for duplicate type declarations.
instance DuplicateError (Name PStage Types) (TypeDecl Parse) where
  duplicateError = errorMultipleDeclarations

-- | Message for a duplicated top-level value declaration. This includes both
-- constrcutor names between two declarations, and top-level functions.
instance
  DuplicateError
    (Name PStage Values)
    (Either (ConstructorDecl Parse) (ValueDecl Parse))
  where
  duplicateError = errorMultipleDeclarations

instance DuplicateError (Name PStage Values) (SignatureDecl Parse) where
  duplicateError = errorMultipleDeclarations

-- | Message for a duplicated constructor inside a type declaration.
instance DuplicateError (Name PStage Values) (Pos, [PType]) where
  duplicateError v (p1, _) (p2, _) = errorMultipleDeclarations v p1 p2

-- | Message for a duplicate branch in a case expression:
--
-- > case … of { A -> …, A -> … }
instance DuplicateError (Name PStage Values) (E.CaseBranch f Parse) where
  duplicateError = errorDuplicateBranch

-- | Messages for any form of duplicate binding:
--
-- * patterns
-- * lambda abstractions (not yet implemented)
-- * type parameters
-- * top-level function parameters
instance ErrorMsg a => DuplicateError a Pos where
  duplicateError = errorDuplicateBind

errorMultipleDeclarations ::
  (ErrorMsg a, HasPos p1, HasPos p2) => a -> p1 -> p2 -> Diagnostic
errorMultipleDeclarations a (pos -> p1) (pos -> p2) =
  PosError
    (max p1 p2)
    [ Error "Multiple declarations of",
      Error a,
      ErrLine,
      Error "Declared at:",
      Error (min p1 p2),
      ErrLine,
      Error "            ",
      Error (max p1 p2)
    ]

errorDuplicateBind :: ErrorMsg v => v -> Pos -> Pos -> Diagnostic
errorDuplicateBind name p1 p2 =
  PosError
    (min p1 p2)
    [ Error "Conflicting bindings for",
      Error name,
      ErrLine,
      Error "Bound at:",
      Error (min p1 p2),
      ErrLine,
      Error "         ",
      Error (max p1 p2)
    ]
{-# NOINLINE errorDuplicateBind #-}

errorDuplicateBranch :: ProgVar PStage -> E.CaseBranch f x -> E.CaseBranch f x -> Diagnostic
errorDuplicateBranch name (pos -> p1) (pos -> p2) =
  PosError
    (max p1 p2)
    [ Error "Duplicate case alternative",
      Error name,
      ErrLine,
      Error "Branches at:",
      Error (min p1 p2),
      ErrLine,
      Error "            ",
      Error (max p1 p2)
    ]

errorImportShadowed :: Pos -> ProgVar PStage -> Diagnostic
errorImportShadowed loc name =
  PosError
    loc
    [ Error "Declaration of",
      Error name,
      Error "shadows an import/builtin of the same name."
    ]
{-# NOINLINE errorImportShadowed #-}

-- | An error message for when a lambda binds only type variables but uses the
-- linear arrow @-o@. This combination does not make sense, therefore we do not
-- allow it.
errorNoTermLinLambda :: Pos -> Pos -> Diagnostic
errorNoTermLinLambda lambdaLoc arrowLoc =
  PosError
    arrowLoc
    [ Error "Lambda at",
      Error lambdaLoc,
      Error "binds only type variables.",
      ErrLine,
      Error "Use the unrestricted arrow ‘->’ for this case."
    ]
{-# NOINLINE errorNoTermLinLambda #-}

errorRecNoTermLambda :: Pos -> Diagnostic
errorRecNoTermLambda p = PosError p [Error msg1, ErrLine, Error msg2]
  where
    msg1 =
      "• a ‘rec’ expression's right-hand side must consist of a \
      \lambda abstraction."
    msg2 =
      "• a ‘rec’ expression must bind at least one non-type parameter in \
      \their right-hand side lambda abstraction."
{-# NOINLINE errorRecNoTermLambda #-}

errorMultipleWildcards ::
  E.CaseBranch Identity Parse -> E.CaseBranch Identity Parse -> Diagnostic
errorMultipleWildcards x y =
  PosError
    (pos b2)
    [ Error "Multiple wildcards in case expression:",
      Error $ runIdentity $ E.branchBinds b1,
      Error $ runIdentity $ E.branchBinds b2,
      ErrLine,
      Error "Branches at:",
      Error (pos b1),
      ErrLine,
      Error "            ",
      Error (pos b2)
    ]
  where
    (b1, b2) =
      if pos x < pos y
        then (x, y)
        else (y, x)

errorMisplacedPairCon ::
  forall (s :: Scope) proxy. SingI s => Pos -> proxy s -> Diagnostic
errorMisplacedPairCon loc _ =
  PosError
    loc
    [ Error "Pair constructor",
      Error $ PairCon @s,
      Error "cannot be used as",
      Error . id @String $
        eitherName @s
          "a type constructor."
          "an expression.",
      ErrLine,
      Error "It can only appear in patterns and with ‘select’."
    ]
{-# NOINLINE errorMisplacedPairCon #-}

errorConflictingImports ::
  Pos ->
  ImportKey ->
  Located ImportBehaviour ->
  Located ImportBehaviour ->
  Diagnostic
errorConflictingImports importLoc (scope, name) i1 i2 =
  PosError (max (pos i1) (pos i2)) . List.intercalate [ErrLine] $
    [ [ Error $
          "Conflicting imports of" ++ case scope of
            Types -> " type"
            Values -> "",
        Error name
      ],
      describe i1,
      describe i2,
      [Error "In import statement at", Error importLoc]
    ]
  where
    describe (p :@ i) = case i of
      ImportHide ->
        [Error "    hidden at", Error p]
      ImportAsIs ->
        [Error "    imported at", Error p]
      ImportFrom orig ->
        [Error "    renamed from", Error orig, Error "at", Error p]
{-# NOINLINE errorConflictingImports #-}

errorInvalidKind :: Pos -> String -> Diagnostic
errorInvalidKind p s = PosError p [Error "Invalid kind", Error (Unqualified s)]
{-# NOINLINE errorInvalidKind #-}
