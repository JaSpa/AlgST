{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module AlgST.Rename
  ( -- * Aliases and Types
    module AlgST.Rename.Phase,

    -- * Renaming
    RnM,
    Globals,
    ModuleMap (..),
    TopLevels (..),
    _TopLevels,
    renameModule,
    renameModuleExtra,
    continueRename,
    RenameExtra (..),
    renameSyntax,

    -- * Handling imports
    RenameEnv (..),
    Bindings (..),
    _Bindings,
    PartialResolve (..),
    ResolvedImport,
    importedRenameEnv,
    foldImportedRenameEnv,
    importAllEnv,
  )
where

import AlgST.Builtins.Names qualified as Builtin
import AlgST.Parse.Phase
import AlgST.Rename.Error (MonadErrors, addError, fatalError)
import AlgST.Rename.Error qualified as Error
import AlgST.Rename.Fresh
import AlgST.Rename.Modules
import AlgST.Rename.Monad
import AlgST.Rename.Operators
import AlgST.Rename.Phase
import AlgST.Syntax.Decl qualified as D
import AlgST.Syntax.Expression qualified as E
import AlgST.Syntax.Module
import AlgST.Syntax.Name
import AlgST.Syntax.Traversal
import AlgST.Syntax.Type qualified as T
import AlgST.Util
import AlgST.Util.ErrorMessage (DErrors)
import AlgST.Util.Lenses
import AlgST.Util.Lenses qualified as L
import AlgST.Util.SourceLocation (needPos)
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Validate
import Data.Bitraversable
import Data.Coerce
import Data.DList qualified as DL
import Data.Functor.Compose
import Data.HashMap.Strict qualified as HM
import Data.Map qualified as Map
import Data.Maybe
import Data.Monoid
import Data.Singletons
import Instances.TH.Lift ()
import Lens.Family2
import Lens.Family2.State.Strict ((%=))
import Lens.Family2.Stock

-- | For a @ResolvedImport@ the 'importTarget' has to point to the imported
-- module's name (used for error messages) and the imported module's
-- 'ModuleMap'.
type ResolvedImport = Import (ModuleName, ModuleMap)

-- | Merges the 'RenameEnv's extracted from all given imports. See
-- 'importedRenameEnv' for more information.
foldImportedRenameEnv :: (MonadErrors m) => [Located ResolvedImport] -> m RenameEnv
foldImportedRenameEnv = getAp . foldMap (Ap . importedRenameEnv)

-- | This function builds a 'RenameEnv' from a 'ResolvedImport'. The keys in
-- the returned 'RenameEnv' will have been adjusted based on any renamings and
-- the 'importQualifier'.
--
-- Any hidden/explicitly imported/renamed identifiers which do not exist in the
-- imported module will be diagnosed.
importedRenameEnv ::
  forall m.
  (MonadErrors m) =>
  Located ResolvedImport ->
  m RenameEnv
importedRenameEnv stmt =
  case foldL importSelection stmt of
    ImportAll allLoc hides renames -> do
      -- The allSet contains all identifiers which are not hidden.
      let allSet = allItems allLoc \itemKey -> not $ HM.member itemKey hides
      -- Add all the renamed items on top of the `allSet`.
      getAp $ pure allSet <> HM.foldMapWithKey (coerce addItem) renames
    ImportOnly renames ->
      getAp $ HM.foldMapWithKey (coerce addItem) renames
  where
    (importName, importMap) = importTarget (unL stmt)

    nameHereQ :: Unqualified -> NameW scope
    nameHereQ = Name (foldL importQualifier stmt)

    singleBinding :: forall scope. Pos -> Unqualified -> NameR scope -> Bindings scope
    singleBinding loc unq nameR = do
      Bindings . Map.singleton (nameHereQ unq) $
        resolvedUnique
          (nameR & nameWrittenL .~ nameHereQ unq)
          (Error.AmbiguousImport loc importName)

    allItems :: Pos -> (ImportKey -> Bool) -> RenameEnv
    allItems allLoc include = do
      let item :: forall scope. (SingI scope) => Unqualified -> NameR scope -> Bindings scope
          item unqualified nameR =
            mguard
              (include (demote @scope, unqualified))
              (singleBinding allLoc unqualified nameR)
      let mkBindings :: forall scope. (SingI scope) => Bindings scope
          mkBindings = HM.foldMapWithKey item $ importMap ^. scopeL . _TopLevels
      RenameEnv {rnTyVars = mkBindings, rnProgVars = mkBindings}

    addItem :: ImportKey -> Located Unqualified -> m RenameEnv
    addItem (scope, nameHere) item@(_ :@ nameThere) = withSomeSing scope \sscope -> do
      let resolvedItem =
            importMap
              ^. scopeL' sscope
                . _TopLevels
                . L.hashAt nameThere
      let unknownItemErr =
            Error.unknownImportItem
              (pos stmt)
              (foldL (fst . importTarget) stmt)
              scope
              item
      case resolvedItem of
        Nothing -> do
          mempty <$ addError unknownItemErr
        Just nameThereR -> do
          let binding = singleBinding (pos item) nameHere nameThereR
          pure $ mempty & scopeL' sscope .~ binding

-- | A simplified version of 'importedRenameEnv' which does no renaming and no
-- hiding. This allows us to guarantee that no errors will occur.
importAllEnv :: Pos -> ModuleName -> ModuleMap -> ModuleName -> RenameEnv
importAllEnv loc targetName targetMap qualifier =
  RenameEnv
    { rnTyVars = mkBindings,
      rnProgVars = mkBindings
    }
  where
    mkBindings :: forall scope. (SingI scope) => Bindings scope
    mkBindings =
      targetMap
        ^. scopeL
          . _TopLevels
          . to HM.toList
          . traverse
          . to (uncurry singleBinding)

    singleBinding :: forall scope. Unqualified -> NameR scope -> Bindings scope
    singleBinding unq nameR = do
      Bindings . Map.singleton (Name qualifier unq) $
        resolvedUnique
          (nameR & nameWrittenL .~ Name qualifier unq)
          (Error.AmbiguousImport loc targetName)

instance SynTraversal RnM Parse Rn where
  typeVariable _ x = fmap (T.Var x) . lookupName Error.Var x
  valueVariable _ x = fmap (E.Var x) . lookupName Error.Var x
  bind _ vs = withBindings \f -> traverse f vs

  useConstructor _ loc w = case singByProxy w of
    -- Special check for value constructors: We have to resolve `(,)` to
    -- `conPair`.
    SValues | w == nameWritten Builtin.conPair -> pure Builtin.conPair
    _ -> lookupName Error.Con loc w

  exprExtension pxy = \case
    Left parsedBuiltin ->
      E.Exp <$> traverseSyntax pxy parsedBuiltin
    Right ops -> do
      rnOps <- traverseSyntax pxy ops
      rewriteOperatorSequence rnOps

  typeExtension pxy = fmap T.Type . traverseSyntax pxy

lookupName :: (SingI scope) => Error.NameKind -> Pos -> NameW scope -> RnM (NameR scope)
lookupName kind loc w = do
  resolve <- asks . view $ _2 . scopeL . _Bindings . at w
  case viewUniqueResolve <$> resolve of
    Nothing ->
      fatalError $ Error.unboundName loc kind w
    Just (Right r) ->
      pure r
    Just (Left choices) ->
      fatalError $ Error.ambiguousUsage loc kind w choices

bindingParams :: D.Params PStage -> (D.Params RnStage -> RnM a) -> RnM a
bindingParams params = withBindings \f ->
  traverse (bitraverse (traverse f) pure) params

bindingANames ::
  (Traversable f) => f (ANameG Written) -> (f (ANameG Resolved) -> RnM a) -> RnM a
bindingANames vs = withBindings \bind ->
  traverse (bitraverse bind bind) vs
{-# INLINEABLE bindingANames #-}

withBindings ::
  (forall f. (Applicative f) => (forall s. (SingI s) => PName s -> f (RnName s)) -> f a) ->
  (a -> RnM b) ->
  RnM b
withBindings f k = do
  let bind :: (SingI s) => PName s -> StateT RenameEnv Fresh (RnName s)
      bind v = do
        v' <- lift $ freshResolved v
        scopeL . _Bindings %= Map.insert v (UniqueLocal v')
        pure v'
  (a, binds) <- lift $ lift $ runStateT (f bind) mempty

  -- It is important that `binds` appears on the LEFT side of the `(<>)`
  -- operator! When unioning the underlying maps prefer values from the left
  -- operand. The new bindings in `binds` have to shadow/replace older
  -- bindings.
  local (fmap (binds <>)) (k a)

newtype RenameExtra = RenameExtra (forall a. (RnModule -> RnM a) -> Either DErrors a)

type RenameResult a = (ModuleMap, RenameEnv -> Either DErrors a)

renameModule :: ModuleName -> RenameEnv -> PModule -> Either DErrors RnModule
renameModule name env m = do
  let (_, rn) = renameModuleExtra name m
  rn env >>= \(RenameExtra extra) -> extra pure

renameModuleExtra :: ModuleName -> PModule -> RenameResult RenameExtra
renameModuleExtra = continueRename emptyModuleMap

-- | Given a partial module map (base map) this function “continues” the rename
-- of the given model in the sense that any top-level identifiers are first
-- looked for in the base map before a fresh name is generated.
--
-- This allows to refer to top-level identifiers before the whole module is
-- renamed. Also, it allows you to generate stable (resolved) identifiers which
-- are valid after renaming.
continueRename :: ModuleMap -> ModuleName -> PModule -> RenameResult RenameExtra
continueRename baseMap moduleName m =
  -- Renaming traverses the module top-levels twice. Overall this is acceptable
  -- but it might be possible using laziness and recrursive definitions to
  -- reduce it to one traversal.
  -- The difficult part, however, is to ensure that it is not possible to
  -- get stuck in an infinite recursion when some part is to strict.
  (toplevels, renameBodies)
  where
    firstId = do
      let idsOf = views (_TopLevels . traverse . nameResolvedIdL) DL.singleton
      case DL.toList $ idsOf (modMapTypes baseMap) <> idsOf (modMapValues baseMap) of
        [] -> firstResolvedId
        ids -> nextResolvedId $ maximum ids
    ((toplevels, localsEnv), nextRid) =
      moduleTopLevels baseMap m
        & unFresh
        & flip runReaderT moduleName
        & flip runState firstId
    renameBodies baseEnv = do
      pure $ RenameExtra \k ->
        (doRename m >>= k)
          & runValidateT
          & flip runReaderT (toplevels, localsEnv <> baseEnv)
          & unFresh
          & flip runReaderT moduleName
          & flip evalState nextRid

moduleTopLevels :: ModuleMap -> PModule -> Fresh (ModuleMap, RenameEnv)
moduleTopLevels baseMap m = do
  let entry ::
        -- (SingI scope, HasRange a) =>
        (SingI scope) =>
        NameW scope ->
        a ->
        Fresh ((Unqualified, NameR scope), PartialResolve scope)
      entry nameW decl = do
        -- Only generate a fresh name if there isn't already a resolved version
        -- in `baseMap`.
        nameR <- case baseMap ^. scopeL . _TopLevels . hashAt (nameUnqualified nameW) of
          Nothing -> freshResolved nameW
          Just n -> pure n
        pure
          ( (nameUnqualified nameW, nameR),
            resolvedUnique nameR $ Error.AmbiguousDefine $ needPos decl
          )
  typs <- Map.traverseWithKey entry $ moduleTypes m
  vals <- Map.traverseWithKey entry $ moduleValues m
  sigs <- Map.traverseWithKey entry $ moduleSigs m
  pure
    ( ModuleMap
        { modMapTypes =
            TopLevels . HM.fromList $
              fst <$> Map.elems typs,
          modMapValues =
            TopLevels . HM.fromList $
              (fst <$> Map.elems sigs) ++ (fst <$> Map.elems vals)
        },
      RenameEnv
        { rnTyVars = Bindings $ fmap snd typs,
          rnProgVars = Bindings $ fmap snd (vals <> sigs)
        }
    )

doRename :: PModule -> RnM RnModule
doRename m = do
  typs <- renameGlobalNameMap renameTypeDecl (moduleTypes m)
  sigs <- renameGlobalNameMap renameSigDecl (moduleSigs m)
  vals <-
    renameGlobalNameMap
      (bitraverse renameConDecl renameValDecl)
      (moduleValues m)
  benches <-
    traverse
      (\(Benchmark n x t1 t2) -> Benchmark n x <$> renameSyntax t1 <*> renameSyntax t2)
      (moduleBench m)
  pure
    Module
      { moduleTypes = typs,
        moduleValues = vals,
        moduleSigs = sigs,
        moduleBench = benches
      }

renameSyntax :: (SynTraversable Parse Rn (s Parse) (s Rn)) => s Parse -> RnM (s Rn)
renameSyntax = traverseSyntaxBetween (Proxy @Parse) (Proxy @Rn)

renameGlobalNameMap ::
  (SingI scope) =>
  (a -> RnM a') ->
  NameMapG Written scope a ->
  RnM (NameMapG Resolved scope a')
renameGlobalNameMap rn m = do
  tls <- asks fst
  fmap Map.fromList . sequenceA $
    [ (nameR,) <$> rn a
    | (nameUnqualified -> u, a) <- Map.toList m,
      nameR <- tls ^.. scopeL . _TopLevels . hashAt u . traverse
    ]

renameTypeDecl :: D.TypeDecl Parse -> RnM (D.TypeDecl Rn)
renameTypeDecl = \case
  D.AliasDecl x alias ->
    D.AliasDecl x <$> renameAlias alias
  D.DataDecl x decl ->
    D.DataDecl x <$> renameNominal renameSyntax decl
  D.ProtoDecl x decl ->
    D.ProtoDecl x <$> renameNominal renameSyntax decl

renameAlias :: D.TypeAlias Parse -> RnM (D.TypeAlias Rn)
renameAlias alias = bindingParams (D.aliasParams alias) \ps -> do
  ty <- renameSyntax (D.aliasType alias)
  pure
    D.TypeAlias
      { aliasParams = ps,
        aliasType = ty,
        aliasKind = D.aliasKind alias
      }

renameNominal :: (a -> RnM b) -> D.TypeNominal PStage a -> RnM (D.TypeNominal RnStage b)
renameNominal f nom = bindingParams (D.nominalParams nom) \ps -> do
  cs <-
    renameGlobalNameMap
      (traverse (traverse f))
      (D.nominalConstructors nom)
  pure
    D.TypeNominal
      { nominalParams = ps,
        nominalConstructors = cs,
        nominalKind = D.nominalKind nom
      }

renameSigDecl :: D.SignatureDecl Parse -> RnM (D.SignatureDecl Rn)
renameSigDecl (D.SignatureDecl x ty) = D.SignatureDecl x <$> renameSyntax ty

renameConDecl :: D.ConstructorDecl Parse -> RnM (D.ConstructorDecl Rn)
renameConDecl = \case
  D.DataCon x parent params mul items -> do
    parentR <- resolveParent parent
    (params', items') <- bindingParams params \params' ->
      (params',) <$> traverse renameSyntax items
    pure $ D.DataCon x parentR params' mul items'
  D.ProtocolCon x parent params items -> do
    parentR <- resolveParent parent
    (params', items') <- bindingParams params \params' ->
      (params',) <$> traverse renameSyntax items
    pure $ D.ProtocolCon x parentR params' items'
  where
    resolveParent p =
      asks . view $
        _1
          . scopeL
          . _TopLevels
          . hashAt (nameUnqualified p)
          . to (fromMaybe $ error $ "DataCon parent name not found: " ++ pprName p)

renameValDecl :: D.ValueDecl Parse -> RnM (D.ValueDecl Rn)
renameValDecl vd =
  bindingANames (Compose $ D.valueParams vd) \(Compose ps) -> do
    t <- renameSyntax (D.valueType vd)
    e <- renameSyntax (D.valueBody vd)
    pure vd {D.valueParams = ps, D.valueType = t, D.valueBody = e}
