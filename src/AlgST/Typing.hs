{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module AlgST.Typing
  ( -- * Monad Stack
    TcM,
    runTcM,
    runErrors,
    checkResultAsRnM,

    -- * Kinds
    HasKiEnv,
    HasKiSt,
    KindEnv,
    KiSt,
    KiTypingEnv,
    kisynth,
    kicheck,
    bindTyVar,
    bindTyVars,

    -- * Types
    TypeM,
    runTypeM,
    TypeEnv,
    TySt,
    TyTypingEnv,
    TcValue,
    Var,
    Usage,
    tysynth,
    tycheck,
    normalize,

    -- * Programs
    RunTyM,
    checkModule,
    checkWithModule,
    checkSignature,
    CheckContext,
    extractCheckContext,

    -- * Phase
    Tc,
    TcExp,
    TcExpX (..),
    TcType,
    TcBind,
    TcCaseMap,
    TcModule,
  )
where

import AlgST.Builtins.Names
import AlgST.Parse.Phase
import AlgST.Rename
import AlgST.Rename.Fresh
import AlgST.Syntax.Decl
import AlgST.Syntax.Expression qualified as E
import AlgST.Syntax.Kind qualified as K
import AlgST.Syntax.Module
import AlgST.Syntax.Name
import AlgST.Syntax.Traversal
import AlgST.Syntax.Type qualified as T
import AlgST.Typing.Align
import AlgST.Typing.Error qualified as Error
import AlgST.Typing.Monad
import AlgST.Typing.NormalForm
import AlgST.Typing.Phase
import AlgST.Util
import AlgST.Util.ErrorMessage hiding (Errors)
import AlgST.Util.PartialOrd
import Control.Applicative
import Control.Category ((>>>))
import Control.Monad
import Control.Monad.Eta
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import Control.Monad.Validate
import Data.Bifunctor
import Data.DList.DNonEmpty qualified as DNE
import Data.Foldable
import Data.Functor.Identity
import Data.List.NonEmpty (NonEmpty (..), (<|))
import Data.List.NonEmpty qualified as NE
import Data.Map.Merge.Strict qualified as Merge
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Semigroup
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.These
import Data.Tuple qualified as Tuple
import Data.Void
import Language.Haskell.TH.Syntax (Lift)
import Lens.Family2
import Lens.Family2.State.Strict
import Lens.Family2.Stock (at')

-- | Translates the typechecker specific error set representation into a simple
-- list.
runErrors :: Errors -> DErrors
runErrors = mergeTheseWith id recursiveErrs (<>)
  where
    recursiveErrs (RecursiveSets oneKeys oneRec recs) =
      DNE.fromNonEmpty $
        Error.cyclicAliases oneRec
          :| fmap Error.cyclicAliases (Map.elems (Map.delete oneKeys recs))

runTypeM ::
  TyTypingEnv -> TySt -> TypeM a -> Fresh (TySt, Either Errors a)
runTypeM = runTcM

runTcM ::
  env -> st -> TcM env st a -> Fresh (st, Either Errors a)
runTcM typesEnv s m =
  Tuple.swap <$> runReaderT (runStateT (runValidateT m) s) typesEnv

type RunTyM env st = forall a. TypeM a -> TcM env st a

data CheckContext = CheckContext
  { contextTypes :: !(TypesMap Tc),
    contextTyCons :: !(TcNameMap Types TypeCon),
    contextValues :: !(TcNameMap Values TcValue)
  }
  deriving stock (Lift)

instance Semigroup CheckContext where
  CheckContext a1 b1 c1 <> CheckContext a2 b2 c2 =
    CheckContext (a1 <> a2) (b1 <> b2) (c1 <> c2)
  stimes = stimesIdempotentMonoid

instance Monoid CheckContext where
  mempty = CheckContext mempty mempty mempty

extractCheckContext :: TypeM CheckContext
extractCheckContext = do
  tycons <- gets $ view $ kiStL . tcTypeConsL
  tyEnv <- ask
  pure
    CheckContext
      { contextTypes = tcCheckedTypes tyEnv,
        contextValues = tcCheckedValues tyEnv,
        contextTyCons = tycons
      }

checkWithModule ::
  CheckContext ->
  RnModule ->
  (forall env st. (HasKiEnv env, HasKiSt st) => RunTyM env st -> TcModule -> TcM env st r) ->
  ValidateT Errors Fresh r
checkWithModule ctxt prog k = do
  (st, (tcTypes, tcValues, tcSigs, tcBench)) <- run kiSt0 checkGlobals
  let sigValues = Map.map (ValueGlobal Nothing . signatureType) tcSigs
  let tyEnv =
        TyTypingEnv
          { tcCheckedTypes = contextTypes ctxt <> tcTypes,
            tcCheckedValues = contextValues ctxt <> tcValues <> sigValues,
            tcKiTypingEnv = emptyKiTypingEnv -- See [Note: Empty KiTypingEnv]
          }
  let embed :: TypeM a -> TcM env KiSt a
      embed = embedTypeM tyEnv
  let mkProg :: ValuesMap Tc -> TcModule
      mkProg values =
        Module
          { moduleTypes = tcTypes,
            moduleValues = values,
            moduleSigs = tcSigs,
            moduleBench = tcBench
          }
  (st', prog) <- run st $ mkProg <$> checkValueBodies embed tcValues
  fmap snd $ run st' $ k (embedTypeM tyEnv) prog
  where
    kiSt0 =
      KiSt
        { tcTypeCons =
            contextTyCons ctxt
              <> typeConstructorsFromDecls (moduleTypes prog)
        }
    run st m = do
      (res, st) <-
        runValidateT m
          & flip runStateT st
          & flip runReaderT emptyKiTypingEnv
          & lift
      (st,) <$> either refute pure res
    checkGlobals = do
      checkAliases
      tcTypes <- checkTypeDecls (moduleTypes prog)
      checkedDefs <- checkValueSignatures (moduleValues prog)
      tcSigs <- traverse checkSignature (moduleSigs prog)
      tcBench <- traverse checkBench (moduleBench prog)
      pure (tcTypes, checkedConstructors tcTypes <> checkedDefs, tcSigs, tcBench)
    checkBench bench = do
      (tc1, k1) <- kisynth (benchT1 bench)
      (tc2, k2) <- kisynth (benchT2 bench)
      when (k1 /= k2) do
        Error.add $ Error.benchKindMismatch (pos tc1) k1 (pos tc2) k2
      -- We only check that the types are different for the "BENCHMARK!" case.
      -- If the user wants to ensure that the types are the same it is quite
      -- simple to write two functions witnessing the equality.
      when (not (benchExpect bench)) do
        n1 <- normalize tc1
        n2 <- normalize tc2
        Error.adds [Error.benchTypesEqual (pos tc1) | Alpha n1 == Alpha n2]
      pure bench {benchT1 = tc1, benchT2 = tc2}

{- [Note: Empty KiTypingEnv]

It would be relatively reasonable, when running a TypeM computation, to look
into the current kind context to get a `KiTypingEnv` value. However, at the
moment this is not necessary: although we can start a 'KindM' computation from
inside a 'TypeM' computation the reverse is not possible. In other words: a
threading of the kind checking context through a level of type checking into
kind checking is not necessary.

-}

checkModule :: CheckContext -> Module Rn -> ValidateT Errors Fresh TcModule
checkModule ctxt p = checkWithModule ctxt p \_ -> pure -- `const pure` does not work because of simplified subsumption.

checkResultAsRnM :: ValidateT Errors Fresh a -> RnM a
checkResultAsRnM = mapValidateT lift >>> mapErrors runErrors

useVar :: (MonadValidate Errors m) => Pos -> ProgVar TcStage -> Var -> m Var
useVar loc name v = case varUsage v of
  UnUsage ->
    pure v
  LinUnunsed ->
    pure v {varUsage = LinUsed loc}
  LinUsed ppos -> do
    Error.add (Error.linVarUsedTwice ppos loc name v)
    pure v

checkTypeDecls :: (HasKiEnv env, HasKiSt st) => TypesMap Rn -> TcM env st (TypesMap Tc)
checkTypeDecls = Map.traverseMaybeWithKey checkTypeDecl

-- | Checks the given type declaration. Nothing is done for 'AliasDecl's. The
-- returned map contains the checked constructors.
checkTypeDecl ::
  (HasKiEnv env, HasKiSt st) =>
  TypeVar TcStage ->
  TypeDecl Rn ->
  TcM env st (Maybe (TypeDecl Tc))
checkTypeDecl name = \case
  AliasDecl _ _ ->
    pure Nothing
  DataDecl origin decl -> do
    -- New data declarations are allowed to be TL or TU.
    let allowed = K.TL :| [K.TU]
    kind <- expectNominalKind (pos origin) "data" name (nominalKind decl) allowed
    tcConstructors <- local (bindParams (nominalParams decl)) do
      traverseConstructors (checkDataCon kind) (nominalConstructors decl)
    pure $ Just $ DataDecl origin decl {nominalConstructors = tcConstructors}
  ProtoDecl origin decl -> do
    _ <- expectNominalKind (pos origin) "protocol" name (nominalKind decl) (K.P :| [])
    tcConstructors <- local (bindParams (nominalParams decl)) do
      traverseConstructors (const checkProtoCon) (nominalConstructors decl)
    pure $ Just $ ProtoDecl origin decl {nominalConstructors = tcConstructors}
  where
    checkDataCon k _name field =
      kicheck field k
    checkProtoCon ty = do
      (ty', k) <- kisynth ty
      ty' <$ expectSubkind ty k [K.TL, K.P]

typeConstructorsFromDecls :: TypesMap Rn -> TcNameMap Types TypeCon
typeConstructorsFromDecls = fmap \case
  AliasDecl x ta -> LazyAlias (pos x) ta
  DataDecl _ tn -> NominalTypeCon (nominalParams tn) (nominalKind tn)
  ProtoDecl _ tn -> NominalTypeCon (nominalParams tn) (nominalKind tn)

applyTypeCon ::
  (HasKiEnv env, HasKiSt st) =>
  Pos ->
  NameR Types ->
  TypeCon ->
  [RnType] ->
  TcM env st (TcType, K.Kind)
applyTypeCon loc name con args = case con of
  NominalTypeCon params kind -> do
    subs <- zipTypeParams loc name params args
    let typeRef =
          TypeRef
            { typeRefPos = loc,
              typeRefKind = kind,
              typeRefName = name,
              typeRefArgs = snd <$> subs
            }
    pure (T.Type typeRef, kind)

  -- The alias is already resolved.
  ResolvedAlias alias kind -> do
    useExpanded alias kind

  -- The alias is not yet expanded.
  LazyAlias defLoc alias -> do
    (checked, k) <- expandTypeAlias name defLoc alias
    useExpanded checked k

  -- We already discovered a cycle in this alias.
  CyclicAlias recs -> do
    refute (That recs)

  -- The alias is already being expanded. We discovered a cyclic definition.
  ExpandingAlias depth -> do
    recs <- markCyclicTypeAliases depth
    refute (That recs)
  where
    useExpanded ta k = do
      subs <- zipTypeParams loc name (aliasParams ta) args
      let ty = substituteType (Map.fromList subs) (aliasType ta)
      pure (ty, k)

expandTypeAlias :: (HasKiEnv env, HasKiSt st) => NameR Types -> Pos -> TypeAlias Rn -> TcM env st (TypeAlias Tc, K.Kind)
expandTypeAlias name defLoc alias = do
  depth <- asks $ view kiEnvL >>> tcExpansionStack >>> length
  -- Mark the alias as being expanded.
  kiStL . tcTypeConsL %= Map.insert name (ExpandingAlias depth)
  (expanded, k) <- local (noteExpansion defLoc alias) do
    case aliasKind alias of
      Nothing -> kisynth (aliasType alias)
      Just k -> (,k) <$> kicheck (aliasType alias) k
  let checked =
        alias
          { aliasType = expanded,
            -- Although no change occurs here the record can only change
            -- its type when all the dependent bits change; 'aliasParams'
            -- is at least nominally dependent on the phase token.
            aliasParams = aliasParams alias
          }
  -- Mark the alias as successfully checked.
  kiStL . tcTypeConsL %= Map.insert name (ResolvedAlias checked k)
  pure (checked, k)
  where
    noteExpansion aliasLoc alias env = do
      let expansion =
            ExpansionEntry
              { expansionLoc = aliasLoc,
                expansionName = name,
                expansionAlias = alias
              }
      env
        & kiEnvL . tcExpansionStackL %~ (Seq.|> expansion)
        & bindParams (aliasParams alias)

markCyclicTypeAliases :: (HasKiEnv env, HasKiSt st) => Int -> TcM env st RecursiveSets
markCyclicTypeAliases depth = do
  entries <- asks do
    view kiEnvL
      >>> tcExpansionStack
      >>> toList
      >>> drop depth
  let names = Set.fromList $ expansionName <$> entries
  let recs = RecursiveSets names entries $ Map.singleton names entries
  -- Mark all affected aliases as cyclic.
  let cyclicAliases = Map.fromSet (const (CyclicAlias recs)) names
  kiStL . tcTypeConsL %= Map.union cyclicAliases
  pure recs

-- | Makes sure all aliases are expanded. This step is to make sure all invalid
-- type aliases are diagnosed even when they are not used.
checkAliases :: (HasKiSt st, HasKiEnv env) => TcM env st ()
checkAliases = do
  typeNames <- gets $ view $ kiStL . tcTypeConsL . to Map.keys
  for_ typeNames \name -> do
    typeCon <- gets $ view $ kiStL . tcTypeConsL . at' name
    case typeCon of
      Just (LazyAlias defLoc alias) ->
        void $ expandTypeAlias name defLoc alias
      _ ->
        pure ()

-- | Typechecks the signatures of the contained 'ValueDecl's. The resulting map
-- does not contain any 'ValueCon's. Checked constructors are returned by
-- 'checkTypeDecl'/'checkTypeDecls'.
checkValueSignatures ::
  (HasKiEnv env, HasKiSt st) =>
  ValuesMap Rn ->
  TcM env st TcValuesMap
checkValueSignatures = Map.traverseMaybeWithKey $ const \case
  Left _ -> do
    -- The reason for dropping the construcutors here is twofold:
    -- 1. It duplicates some work from 'checkTypeDecl', which by itself is not that bad.
    -- 2. It would therefore duplicate some error messages.
    pure Nothing
  Right decl ->
    Just . ValueGlobal (Just decl) . signatureType
      <$> checkSignature (valueSignatureDecl decl)

checkedConstructors :: TypesMap Tc -> TcValuesMap
checkedConstructors = Map.foldMapWithKey \name ->
  Map.map ValueCon . declConstructors name

checkSignature ::
  (HasKiEnv env, HasKiSt st) => SignatureDecl Rn -> TcM env st (SignatureDecl Tc)
checkSignature (SignatureDecl x ty) =
  SignatureDecl x <$> kicheck ty K.TU

checkValueBodies ::
  (forall a. TypeM a -> TcM env st a) -> TcValuesMap -> TcM env st (ValuesMap Tc)
checkValueBodies embed = Map.traverseMaybeWithKey \_name -> \case
  ValueCon condecl -> do
    -- Constructors have no body to check.
    pure $ Just $ Left condecl
  ValueGlobal (Just ValueDecl {..}) ty -> do
    -- Align the binds with the values type and check the body.
    body <- embed $ checkAlignedBinds ty valueParams valueBody
    pure $ Just $ Right ValueDecl {valueType = ty, valueBody = body, ..}
  ValueGlobal Nothing _ -> do
    -- Non-global values are not possible on this level.
    pure Nothing

embedTypeM :: TyTypingEnv -> TypeM a -> TcM env KiSt a
embedTypeM env m = do
  kist <- get
  let tyst = TySt {tcKindSt = kist, tcTypeEnv = mempty}
  (tyst', res) <- liftFresh $ runTypeM env tyst m
  put (tcKindSt tyst')
  case res of
    Left errs -> refute errs
    Right a -> pure a

checkAlignedBinds :: TcType -> [Located (ANameG TcStage)] -> RnExp -> TypeM TcExp
checkAlignedBinds fullTy allVs e = go fullTy fullTy allVs
  where
    go :: TcType -> TcType -> [Located (ANameG TcStage)] -> TypeM TcExp
    go _ t [] = tycheck e t
    go _ (T.Arrow k mul t u) (p :@ Right pv : vs) = do
      withProgVarBind (unrestrictedLoc k mul) p pv t do
        E.Abs ZeroPos . E.Bind ZeroPos mul pv (Just t) <$> go u u vs
    go _ t0@(T.Forall _ (K.Bind _ sigVar k t)) vs0@(p :@ v : vs) = do
      let subBind tv = substituteType @Tc (Map.singleton sigVar (T.Var (p @- k) tv))
      let wrapAbs tv = E.TypeAbs @Tc ZeroPos . K.Bind p tv k
      case v of
        Left tv -> do
          local (bindTyVar tv k) do
            wrapAbs tv <$> go t (subBind tv t) vs
        Right _ -> do
          -- Skip the type abstraction by binding a wildcard pattern. Keep the
          -- expected type for the error message.
          wildTV <- liftFresh $ freshResolved Wildcard
          wrapAbs wildTV <$> go t0 (subBind wildTV t) vs0
    go t _ (v : _) = do
      -- The bind ›v‹ does not align with the expected type. While trying to
      -- align it the expected type got more and more destructured. The first
      -- argument to 'go' is the un-destructured expected type.
      Error.fatal $ uncurryL Error.mismatchedBind v t

expectNominalKind ::
  (MonadValidate Errors m) =>
  Pos ->
  String ->
  TypeVar TcStage ->
  K.Kind ->
  NonEmpty K.Kind ->
  m K.Kind
expectNominalKind loc nomKind name actual allowed = do
  if actual `elem` allowed
    then pure actual
    else NE.last allowed <$ Error.add (Error.invalidNominalKind loc nomKind name actual allowed)

-- | @typeAppBase t us@ destructures @t@ multiple levels to the type
-- application head. @us@ is a non-empty list of already established
-- parameters.
--
-- The "head" must be a type constructor because the type system does not
-- support higher-kinded types.
--
-- The result is a tuple consisting of the head location, the head, and
-- parameters (including @us@).
typeAppBase ::
  forall env st.
  RnType ->
  NonEmpty RnType ->
  TcM env st (Pos, TypeVar TcStage, [RnType])
typeAppBase = flip go
  where
    go :: NonEmpty RnType -> RnType -> TcM env st (Pos, TypeVar TcStage, [RnType])
    go us = \case
      T.Con p c -> pure (p, c, toList us)
      T.App _ t u -> go (u <| us) t
      t -> Error.fatal $ err us t
    err :: NonEmpty RnType -> RnType -> Diagnostic
    err us t = Error.typeConstructorNParams (pos t) (t <| us) (length us) 0

kisynthTypeCon ::
  (HasKiEnv env, HasKiSt st) =>
  Pos ->
  TypeVar TcStage ->
  [RnType] ->
  TcM env st (TcType, K.Kind)
kisynthTypeCon loc name args = do
  mcon <- gets $ view $ kiStL . tcTypeConsL . at' name
  case mcon of
    Nothing ->
      error $
        show loc
          ++ ": internal error: unknown type constructor "
          ++ pprName name
    Just con ->
      applyTypeCon loc name con args

-- | Kind-checks the types against the parameter list. In case they differ in
-- length an error is emitted.
zipTypeParams ::
  (HasKiEnv env, HasKiSt st) =>
  Pos ->
  TypeVar TcStage ->
  Params TcStage ->
  [RnType] ->
  TcM env st [(TypeVar TcStage, TcType)]
zipTypeParams loc name ps0 ts0 = go 0 ps0 ts0
  where
    go !_ [] [] = pure []
    go !n ((_ :@ v, k) : ps) (a : as) =
      liftA2
        (:)
        ((v,) <$> kicheck a k)
        (go (n + 1) ps as)
    go !n ps as =
      Error.fatal $
        Error.typeConstructorNParams
          loc
          (T.Con loc name :| ts0)
          (n + length as)
          (n + length ps)

-- | Builds a substitutions map based on 'declParams' and 'typeRefArgs'.
typeRefSubstitutions :: TypeDecl Tc -> TypeRef -> Substitutions Tc
typeRefSubstitutions decl ref =
  typeSubstitions . Map.fromList $
    zip (unL . fst <$> declParams decl) (typeRefArgs ref)

-- | Applies a substitution to a 'TypeDecl'.
--
-- /Note:/ The substitution only applies to the constructors and their items. Most notably the
substituteTypeConstructors :: Substitutions Tc -> TypeDecl Tc -> Constructors TcStage TcType
substituteTypeConstructors subs = nomDecl >>> substituteConstructors
  where
    nomDecl = \case
      AliasDecl x _ -> absurd x
      DataDecl _ d -> d
      ProtoDecl _ d -> d
    substituteConstructors nom =
      mapConstructors (const $ applySubstitutions subs) (nominalConstructors nom)

kisynth ::
  (HasKiEnv env, HasKiSt st) =>
  RnType ->
  TcM env st (TcType, K.Kind)
kisynth =
  etaTcM . \case
    T.Unit p -> do
      pure (T.Unit p, K.TU)
    T.Var p v -> do
      mk <- asks $ view kiEnvL >>> tcKindEnv >>> Map.lookup v
      k <- Error.ifNothing (Error.unboundVar p v) mk
      pure (T.Var (p @- k) v, k)
    T.Con p v -> do
      kisynthTypeCon p v []
    T.App _ t u -> do
      (p, c, args) <- typeAppBase t (pure u)
      kisynthTypeCon p c args
    T.Arrow p m t1 t2 -> do
      (t1', t2') <-
        -- Applicatively to get error messages from both branches.
        (,)
          <$> kicheck t1 K.TL
          <*> kicheck t2 K.TL
      let k = K.Kind K.Top m
      pure (T.Arrow p m t1' t2', k)
    T.Forall _ (K.Bind p v k t) -> do
      local (bindTyVar v k) do
        (t', tyk) <- kisynth t
        case K.multiplicity tyk of
          Just m -> do
            let forallK = K.Kind K.Top m
                forallT = T.Forall p (K.Bind p v k t')
            pure (forallT, forallK)
          Nothing -> do
            Error.fatal (Error.unexpectedKind t tyk [])
    T.Pair p t1 t2 -> do
      ((t1', k1), (t2', k2)) <- do
        -- Applicatively to get error messages from both branches.
        (,)
          <$> kisynth t1
          <*> kisynth t2

      -- Make sure none of the components have kind P.
      expectSubkind t1 k1 [K.TL]
      expectSubkind t2 k2 [K.TL]

      -- Calculate the pair's kind:   k1 |_| k2 |_| TU
      --
      -- The TU is there so that a pair does not get a M* kind. See section
      -- "Kind polymorphism for pairs".
      let k = K.leastUpperBound (K.leastUpperBound k1 k2) K.TU
      pure (T.Pair p t1' t2', k)
    T.End p pol -> do
      pure (T.End p pol, K.SL)
    T.Session p pol t1 t2 -> do
      t1' <- kicheck t1 K.P
      t2' <- kicheck t2 K.SL
      pure (T.Session p pol t1' t2', K.SL)
    T.Dualof p t -> do
      (t', k) <- kisynth t
      checkSubkind t k K.SL
      pure (T.Dualof p t', k)
    T.Negate p t -> do
      t' <- kicheck t K.P
      pure (T.Negate p t', K.P)
    T.Type x ->
      absurd x

kicheck :: (HasKiEnv env, HasKiSt st) => RnType -> K.Kind -> TcM env st TcType
kicheck t k = do
  (t', k') <- kisynth t
  expectSubkind t k' [k]
  pure t'

checkSubkind :: (MonadValidate Errors m) => RnType -> K.Kind -> K.Kind -> m ()
checkSubkind t k1 k2 = errorIf (k1 </=? k2) (Error.unexpectedKind t k1 [k2])

tysynth :: RnExp -> TypeM (TcExp, TcType)
tysynth =
  etaTcM . \case
    E.Lit p l -> do
      let t = litType p l
      pure (E.Lit p l, t)

    --
    E.Pair p e1 e2 -> do
      ((e1', t1), (e2', t2)) <-
        (,)
          <$> tysynth e1
          <*> tysynth e2

      let t = T.Pair p t1 t2
      pure (E.Pair p e1' e2', t)

    --
    E.Var p v -> do
      synthVariable p v >>= Error.ifNothing (Error.unboundVar p v)

    --
    E.Con p v -> do
      synthVariable p v >>= Error.ifNothing (Error.undeclaredCon p v)

    --
    E.Abs p bnd -> do
      (bnd', ty) <- tysynthBind p bnd
      pure (E.Abs p bnd', ty)

    --
    E.TypeAbs p bnd -> do
      (bnd', ty) <- tysynthTyBind tysynth p bnd
      pure (E.TypeAbs p bnd', ty)

    --
    E.App p (E.Exp (BuiltinFork _)) e -> do
      -- TODO: Desugar to `fork_`. The hard part is getting the reference to
      -- `send` correct since it is not more than a "imported" signature which
      -- might be shadowed.
      (e', ty) <- tysynth e
      let k = typeKind ty
      when (k </=? K.TL) do
        Error.add $ Error.unexpectedForkKind "fork" e ty k K.TL
      -- Here we can choose between T.In and T.Out: should an expression
      --    fork 1
      -- should have type ?Int.End? or ?Int.End!  ? For now I have decided upon
      -- the former. In theory, this allows the `fork` machinery to eagerly
      -- close the connection. In practice this does not make much of a
      -- difference.
      let resultTy = buildSessionType p T.In [ty] $ T.End p T.In
      pure (E.Fork p e', resultTy)

    --
    E.App p (E.Exp (BuiltinFork_ _)) e -> do
      (e', ty) <- tysynth e
      let k = typeKind ty
      when (k </=? K.TU) do
        Error.add $ Error.unexpectedForkKind "fork_" e ty k K.TU
      pure (E.Fork_ p e', T.Unit p)

    --
    E.App p e1 e2 -> do
      (e1', t1) <- tysynth e1
      (t, u) <-
        Error.ifNothing
          (Error.noArrowType e1 t1)
          (appArrow t1)
      e2' <- tycheck e2 t
      pure (E.App p e1' e2', u)

    --
    E.TypeApp _ (E.Exp (BuiltinNew p)) t -> do
      t' <- kicheck t K.SL
      let newT = T.Pair p t' (T.Dualof p t')
      pure (E.New p t', newT)
    E.TypeApp p e t -> do
      (e', eTy) <- tysynth e
      K.Bind _ v k u <-
        Error.ifNothing
          (Error.noForallType e eTy)
          (appTArrow eTy)
      t' <- kicheck t k
      let u' = substituteType (Map.singleton v t') u
      pure (E.TypeApp p e' t', u')

    --
    E.Rec p v ty r -> do
      -- Check for TU is deliberate here. It is unclear if a linear value would
      -- be well behaved.
      ty' <- kicheck ty K.TU
      withProgVarBind Nothing p v ty' do
        r' <- checkRecLam r ty'
        pure (E.Rec p v ty' r', ty')

    -- 'let' __without__ a type signature.
    E.UnLet p v Nothing e body -> do
      (e', eTy) <- tysynth e
      withProgVarBind Nothing p v eTy do
        (body', bodyTy) <- tysynth body
        let branch =
              E.CaseBranch
                { branchPos = p,
                  branchBinds = Identity (p :@ v),
                  branchExp = body'
                }
        let caseMap =
              E.CaseMap
                { E.casesWildcard = Just branch,
                  E.casesPatterns = Map.empty
                }
        pure (E.Exp $ ValueCase p e' caseMap, bodyTy)

    -- 'let' __with__ a type signature.
    E.UnLet p v (Just ty) e body -> do
      ty' <- kicheck ty K.TL
      e' <- tycheck e ty'
      withProgVarBind Nothing p v ty' do
        (body', bodyTy) <- tysynth body
        let branch =
              E.CaseBranch
                { branchPos = p,
                  branchBinds = Identity (p :@ v),
                  branchExp = body'
                }
        let caseMap =
              E.CaseMap
                { E.casesWildcard = Just branch,
                  E.casesPatterns = Map.empty
                }
        pure (E.Exp $ ValueCase p e' caseMap, bodyTy)

    --
    E.PatLet p c vs e body -> do
      (e', eTy) <- tysynth e
      pat <- extractMatchableType "Pattern let expression" (pos e) eTy
      let branch =
            E.CaseBranch
              { branchPos = pos c,
                branchExp = body,
                branchBinds = vs
              }
      let cases =
            E.CaseMap
              { casesPatterns = Map.singleton (unL c) branch,
                casesWildcard = Nothing
              }
      checkPatternExpr p e' cases pat Nothing

    --
    E.Cond p e eThen eElse -> do
      checkIfExpr p e eThen eElse Nothing

    --
    E.Case p e cases -> do
      (e', eTy) <- tysynth e
      pat <- extractMatchableType "Case expression scrutinee" (pos e) eTy
      checkPatternExpr p e' cases pat Nothing

    --
    E.Select p lcon@(_ :@ con) | con == conPair -> do
      v1 <- freshLocal "a"
      v2 <- freshLocal "b"
      let tyX = T.Var @Tc (p @- kiX)
          kiX = K.TL
      let params = [(p :@ v1, kiX), (p :@ v2, kiX)]
          pairTy = T.Pair ZeroPos (tyX v1) (tyX v2)
      ty <- buildSelectType p params pairTy [tyX v1, tyX v2]
      pure (E.Select p lcon, ty)

    --
    E.Select p lcon@(_ :@ con) -> do
      let findConDecl = runMaybeT do
            ValueCon conDecl <- MaybeT $ asks $ tcCheckedValues >>> Map.lookup con
            parentDecl <- MaybeT $ asks $ tcCheckedTypes >>> Map.lookup (conParent conDecl)
            pure (conDecl, parentDecl)
      (conDecl, parentDecl) <- Error.ifNothing (uncurryL Error.undeclaredCon lcon) =<< findConDecl
      (params, ref) <- instantiateDeclRef ZeroPos (conParent conDecl) parentDecl
      let sub = applySubstitutions (typeRefSubstitutions parentDecl ref)
      ty <- buildSelectType p params (T.Type ref) (sub <$> conItems conDecl)
      pure (E.Select p lcon, ty)

    --
    e@(E.Exp (BuiltinNew _)) -> do
      Error.fatal $ Error.builtinMissingApp e "a type application"
    e@(E.Exp (BuiltinFork _)) -> do
      Error.fatal $ Error.builtinMissingApp e "an expression"
    e@(E.Exp (BuiltinFork_ _)) -> do
      Error.fatal $ Error.builtinMissingApp e "an expression"

    --
    E.New x _ -> absurd x
    E.Fork x _ -> absurd x
    E.Fork_ x _ -> absurd x

buildSelectType ::
  Pos ->
  Params TcStage ->
  TcType ->
  [TcType] ->
  TcM env st TcType
buildSelectType p params t us = do
  varS <- freshLocal "s"
  let tyS = T.Var @Tc (p @- kiS) varS
      kiS = K.SL
  let foralls = buildForallType params . buildForallType [(p :@ varS, kiS)]
  let arrLhs = buildSessionType ZeroPos T.Out [t]
      arrRhs = buildSessionType ZeroPos T.Out us
  let ty = foralls $ T.Arrow p K.Un (arrLhs tyS) (arrRhs tyS)
  pure ty

data PatternType
  = PatternRef TypeRef
  | PatternPair (T.XPair Tc) TcType TcType

-- | Reconstructs the original 'TcType'.
originalPatternType :: PatternType -> TcType
originalPatternType = \case
  PatternRef r -> T.Type r
  PatternPair x t1 t2 -> T.Pair x t1 t2

patternBranches :: PatternType -> TypeM (TcNameMap Values [TcType])
patternBranches = \case
  PatternRef ref -> do
    let subst d = substituteTypeConstructors (typeRefSubstitutions d ref) d
    let missingCon = Error.undeclaredCon (typeRefPos ref) (typeRefName ref)
    mdecl <- asks $ tcCheckedTypes >>> Map.lookup (typeRefName ref)
    fmap snd . subst <$> Error.ifNothing missingCon mdecl
  PatternPair _ t1 t2 ->
    pure $ Map.singleton conPair [t1, t2]

data MatchableType
  = MatchValue PatternType
  | MatchSession PatternType TcType

-- | Checks if a pattern match is possible on a value of the given type. A
-- 'Left' result indicates an ordinary pattern match. A 'Right' result
-- indicates a receiving match on a session type.
--
-- In case the type is not suitable an error is emitted.
extractMatchableType ::
  String -> Pos -> TcType -> TcM env st MatchableType
extractMatchableType s p t = etaTcM do
  tNF <- normalize t
  let patTy = \case
        T.Type ref -> Just $ PatternRef ref
        T.Pair x t1 t2 -> Just $ PatternPair x t1 t2
        _ -> Nothing
  case tNF of
    T.Session _ T.In (patTy -> Just p) s ->
      pure $ MatchSession p s
    (patTy -> Just p) ->
      pure $ MatchValue p
    _ ->
      Error.fatal $ Error.invalidPatternExpr s p t tNF

-- | Looks up the type for the given 'ProgVar'. This function works correctly
-- for local variables, globals and constructors.
synthVariable ::
  Pos -> ProgVar TcStage -> TypeM (Maybe (TcExp, TcType))
synthVariable p name = runMaybeT (useLocal <|> useGlobal)
  where
    useLocal = do
      info <- MaybeT $ gets $ tcTypeEnv >>> Map.lookup name
      used <- useVar p name info
      tcTypeEnvL %= Map.insert name used
      pure (E.Var p name, varType info)

    useGlobal = do
      global <- MaybeT $ asks $ tcCheckedValues >>> Map.lookup name
      case global of
        ValueGlobal _ ty ->
          pure (E.Var p name, ty)
        ValueCon (DataCon _ parent _ mul items) -> do
          decl <- MaybeT $ asks $ tcCheckedTypes >>> Map.lookup parent
          ty <- lift $ buildDataConType p parent decl mul items
          pure (E.Con p name, ty)
        ValueCon (ProtocolCon _ parent _ _) ->
          Error.fatal $ Error.protocolConAsValue p name parent

instantiateDeclRef ::
  Pos -> TypeVar TcStage -> TypeDecl Tc -> TcM env st (Params TcStage, TypeRef)
instantiateDeclRef p name decl = do
  params <- liftFresh $ freshResolvedParams (declParams decl)
  pure
    ( params,
      TypeRef
        { typeRefName = name,
          typeRefKind = tcDeclKind decl,
          typeRefPos = p,
          typeRefArgs = (\(p :@ tv, k) -> T.Var (p @- k) tv) <$> params
        }
    )

-- | Tries to destructure a type into into domain and codomain of an arrow.
-- This includes applying the forall isomorphism to push quantified type
-- variables further down if allowed.
appArrow :: TcType -> Maybe (TcType, TcType)
appArrow = go id Set.empty
  where
    go prependPushed pushed = \case
      T.Arrow _ _ t u
        | not (liftNameSet pushed `anyFree` t) ->
            Just (t, prependPushed u)
      T.Forall x (K.Bind x' v k t) ->
        go
          (prependPushed . T.Forall x . K.Bind x' v k)
          (Set.insert v pushed)
          t
      _ ->
        Nothing

appTArrow :: TcType -> Maybe (K.Bind TcStage TcType)
appTArrow = go id
  where
    go prependArrows = \case
      T.Forall _ (K.Bind x' v k t) ->
        Just (K.Bind x' v k $ prependArrows t)
      T.Arrow x m t u ->
        go (prependArrows . T.Arrow x m t) u
      _ ->
        Nothing

checkIfExpr :: Pos -> RnExp -> RnExp -> RnExp -> Maybe TcType -> TypeM (TcExp, TcType)
checkIfExpr loc eCond eThen eElse mExpectedTy = do
  let boolRef =
        TypeRef
          { typeRefPos = ZeroPos,
            typeRefName = typeBool,
            typeRefArgs = [],
            typeRefKind = K.TU
          }
  eCond' <- tycheck eCond (T.Type boolRef)
  (mresTy, (eThen', eElse')) <- runBranched loc mExpectedTy do
    (,)
      <$> checkBranch (Error.CondThen (pos eThen)) (checkOrSynth eThen)
      <*> checkBranch (Error.CondElse (pos eElse)) (checkOrSynth eElse)
  let resTy = error "checkIfExpr: no result type" `fromMaybe` mresTy

  let branches =
        Map.fromList
          [ (conTrue, E.CaseBranch (pos eThen) [] eThen'),
            (conFalse, E.CaseBranch (pos eElse) [] eElse')
          ]
  let eCase =
        E.Exp . ValueCase loc eCond' $
          E.CaseMap
            { casesPatterns = branches,
              casesWildcard = Nothing
            }
  pure (eCase, resTy)

checkPatternExpr ::
  Pos -> TcExp -> RnCaseMap -> MatchableType -> Maybe TcType -> TypeM (TcExp, TcType)
checkPatternExpr loc scrut cases pat mExpectedTy = do
  case pat of
    MatchSession pat s -> do
      (cases', ty) <- checkSessionCase loc cases pat s mExpectedTy
      pure (E.Exp $ RecvCase loc scrut cases', ty)
    MatchValue pat -> do
      (cases', ty) <- checkDataCase loc cases pat mExpectedTy
      pure (E.Exp $ ValueCase loc scrut cases', ty)

checkDataCase ::
  Pos -> RnCaseMap -> PatternType -> Maybe TcType -> TypeM (TcCaseMap [] Maybe, TcType)
checkDataCase loc cases patTy mExpectedTy =
  checkCaseExpr loc True cases patTy mExpectedTy \con fields b -> etaTcM do
    -- Check that the number of binds matches the number of fields.
    let nGiven = length (E.branchBinds b)
        nExpected = length fields
    errorIf (nGiven /= nExpected) $
      Error.branchPatternBindingCount (pos b) con nExpected nGiven
    -- Return the typed binds.
    pure (zip (E.branchBinds b) fields)

-- | Combines a list of @('TypeVar', 'K.Kind')@ pairs into a nested 'T.Forall'
-- type.
buildForallType :: Params TcStage -> TcType -> TcType
buildForallType = foldMap (Endo . mkForall) >>> appEndo
  where
    mkForall :: (Located (TypeVar TcStage), K.Kind) -> TcType -> TcType
    mkForall (p :@ tv, k) = T.Forall ZeroPos . K.Bind p tv k

buildDataConType ::
  Pos ->
  TypeVar TcStage ->
  TypeDecl Tc ->
  K.Multiplicity ->
  [TcType] ->
  TcM env st TcType
buildDataConType p name decl mul items = do
  (params, ref) <- instantiateDeclRef p name decl
  let subs = typeRefSubstitutions decl ref
  let conArrow = foldr (T.Arrow ZeroPos mul) (T.Type ref) (applySubstitutions subs <$> items)
  pure $ buildForallType params conArrow

buildSessionType :: Pos -> T.Polarity -> [TcType] -> TcType -> TcType
buildSessionType loc pol fields s = foldr (T.Session loc pol) s fields

checkSessionCase ::
  Pos ->
  RnCaseMap ->
  PatternType ->
  TcType ->
  Maybe TcType ->
  TypeM (TcCaseMap Identity (Const ()), TcType)
checkSessionCase loc cases patTy s mExpectedTy = do
  (cmap, ty) <- checkCaseExpr loc False cases patTy mExpectedTy \_con fields b -> etaTcM do
    -- Get the variable to bind and its type.
    let vTy = buildSessionType (pos b) T.In fields s
    v <- Error.ifNothing (Error.invalidSessionCaseBranch b) $ case E.branchBinds b of
      [v] -> Just v
      _ -> Nothing
    pure $ Identity (v, vTy)
  -- A potential wildcard is already diagnosed by 'tysynthCaseExpr'.
  pure (cmap {E.casesWildcard = Const ()}, ty)

checkCaseExpr ::
  forall f.
  (Traversable f) =>
  Pos ->
  Bool ->
  RnCaseMap ->
  PatternType ->
  Maybe TcType ->
  ( ProgVar TcStage ->
    [TcType] ->
    E.CaseBranch [] Rn ->
    TypeM (f (Located (ProgVar TcStage), TcType))
  ) ->
  TypeM (TcCaseMap f Maybe, TcType)
checkCaseExpr loc allowWild cmap patTy mExpectedTy typedBinds = etaTcM do
  allBranches <- patternBranches patTy

  -- Diagnose missing branches / superfluous wildcards.
  let missingBranches = Map.keys $ allBranches `Map.difference` E.casesPatterns cmap
  let hasMissingBranches = not $ null missingBranches
  let hasValidWildcard = allowWild && isJust (E.casesWildcard cmap)
  errorIf (hasMissingBranches && not hasValidWildcard) do
    Error.missingCaseBranches loc missingBranches

  -- Handles a single (written by the user) branch.
  let go ::
        ProgVar TcStage ->
        E.CaseBranch [] Rn ->
        Branched TcType (E.CaseBranch f Tc)
      go con branch = checkBranch (Error.PatternBranch (pos branch) con) \mty -> do
        let branchError =
              Error.mismatchedCaseConstructor
                (pos branch)
                (originalPatternType patTy)
                con
        -- Check that the constructor is valid for the matched type.
        fields <- Error.ifNothing branchError $ Map.lookup con allBranches
        -- Check that the bindings correspond with the fields, establish their
        -- types.
        binds <- typedBinds con fields branch
        -- Check the branch expression in the context of the typed bindings.
        (e, eTy) <- withProgVarBinds Nothing (toList binds) do
          checkOrSynth (E.branchExp branch) mty
        pure (branch {E.branchExp = e, E.branchBinds = fst <$> binds}, eTy)

  let goWild ::
        E.CaseBranch Identity Rn ->
        Branched TcType (E.CaseBranch Identity Tc)
      goWild branch = checkBranch (Error.WildcardBranch $ pos branch) \mty -> do
        errorIf (not hasMissingBranches) $ Error.unnecessaryWildcard (pos branch)
        errorIf (not allowWild) $ Error.wildcardNotAllowed (pos branch) loc
        let binds = (,originalPatternType patTy) <$> E.branchBinds branch
        (e, eTy) <- withProgVarBinds Nothing (toList binds) do
          checkOrSynth (E.branchExp branch) mty
        pure (branch {E.branchExp = e, E.branchBinds = fst <$> binds}, eTy)

  -- Traverse over all written branches.
  (mty, checkedCases) <-
    runBranched loc mExpectedTy $
      E.CaseMap
        <$> Map.traverseWithKey go (E.casesPatterns cmap)
        <*> traverse goWild (E.casesWildcard cmap)

  case mty of
    Nothing -> Error.fatal $ Error.emptyCaseExpr loc
    Just ty -> pure (checkedCases, ty)

-- | The @Branched@ monad builds on top of 'TypeM'. It keeps track of the
-- necessary state to verify that multiple branches have the same type and
-- consume the same set of linear variables. See 'checkBranch' for more
-- information.
--
-- You should refrain from lifting arbitrary 'TypeM' or 'TcM' operations.
newtype Branched r a
  = Branched (ValidateT Errors (StateT (BranchSt r) (ReaderT TypeEnv TypeM)) a)
  deriving newtype (Functor, Applicative, Monad)

data BranchSt r
  = forall b.
  (Error.BranchSpec b) =>
  BranchSt
  { -- | Variables which have been consumed in at least one of the encountered
    -- branches.
    branchesConsumed :: !(TcNameMap Values BranchConsumed),
    -- | A previously encountered branch.
    --
    -- Used in error messages in the likes of /… variable is consumed there
    branchesPrevious :: Maybe b,
    -- | Additional payload the branches have to agree upon.
    branchesResult :: Maybe r
  }

-- | A variable consumed in a branch.
data BranchConsumed = forall b. (Error.BranchSpec b) => BranchConsumed !b !Var !Pos

checkOrSynth :: RnExp -> Maybe TcType -> TypeM (TcExp, TcType)
checkOrSynth e Nothing = tysynth e
checkOrSynth e (Just t) = (,t) <$> tycheck e t

-- | Typecheck a single branch.
--
-- Multiple calls to @checkBranch@ have to agree in the set of consumed linear
-- variables else an error is emitted.
checkBranch ::
  (Error.BranchSpec b) => b -> (Maybe r -> TypeM (a, r)) -> Branched r a
checkBranch thisBranch m = Branched do
  initEnv <- ask
  branchSt <- get
  ((a, r), resultEnv) <- lift . lift . lift $ do
    tcTypeEnvL .= initEnv
    (,) <$> m (branchesResult branchSt) <*> use tcTypeEnvL

  -- Extract the set of consuemd variables relative to before branching and
  -- check that the current branch agrees with any previous branches in which
  -- variables are being consumed.
  let consumed =
        uncurry (BranchConsumed thisBranch)
          <$> filterAdditionalConsumed initEnv resultEnv
  case branchSt of
    BranchSt {branchesPrevious = Nothing} -> do
      -- This is the first branch we check. This sets the precedent which
      -- variables we expected to be consumed.
      pure ()
    BranchSt {branchesPrevious = Just prevBranch} -> do
      -- This is not the first branch we check. Verify that the same set of
      -- variables was consumed as in previous branches.
      checkConsumedOverlap
        (branchesConsumed branchSt)
        prevBranch
        consumed
        thisBranch

  let newBranchSt =
        BranchSt
          { branchesConsumed = branchesConsumed branchSt <> consumed,
            branchesPrevious = Just thisBranch,
            branchesResult = Just r
          }
  put $! newBranchSt
  pure a

runBranched :: Pos -> Maybe r -> Branched r a -> TypeM (Maybe r, a)
runBranched p mr (Branched m) = do
  initEnv <- gets tcTypeEnv
  (a, resultSt) <-
    m
      & embedValidateT
      & flip runStateT initialSt
      & flip runReaderT initEnv
  tcTypeEnvL .= markConsumed p initEnv (branchesConsumed resultSt)
  pure (branchesResult resultSt, a)
  where
    initialSt =
      BranchSt
        { branchesConsumed = mempty,
          branchesPrevious = Nothing @Void,
          branchesResult = mr
        }

-- | @filterAdditionalConsumed env1 env2@ filters @env2@ down to the variables
-- which were not already consumed in @env1@ but are in @env2@.
--
-- @env1@ and @env2@ should be related in that @env2@ should be derived from
-- @env1@. Any variables whicha appear only in one environment are discarded.
filterAdditionalConsumed :: TypeEnv -> TypeEnv -> TcNameMap Values (Var, Pos)
filterAdditionalConsumed =
  Merge.merge Merge.dropMissing Merge.dropMissing (Merge.zipWithMaybeMatched f)
  where
    -- Returns @Just (var, usageLoc)@ iff the variable is unused in the outer
    -- bindings but used in the inner bindings.
    f _ vOuter vInner = do
      -- Uses the 'Maybe' monad, a failing match results in Nothing.
      LinUnunsed <- pure (varUsage vOuter)
      LinUsed loc <- pure (varUsage vInner)
      pure (vInner, loc)

-- | @markConsumed p env vars@ marks all variables in @env@ which appear also
-- as keys in @vars@ as used at @p@. Variables which appear in @vars@ but not
-- in @env@ are ignored.
markConsumed :: Pos -> TypeEnv -> TcNameMap Values a -> TypeEnv
markConsumed !p =
  Merge.merge Merge.preserveMissing Merge.dropMissing (Merge.zipWithMatched mark)
  where
    mark _ v _ = v {varUsage = LinUsed p}

-- | Verifies (or emitts the according errors) that the variables consumed in
-- one branch correspond to the variables consumed in another branch.
--
-- Branches @a@ and @b@ should be the branches from which the preceeding maps
-- originated. These are used in the error messages in the form of "… variable
-- is consumed /here/ but not in branch /{a,b}/ …". We cannot take the from the
-- opposing map because it might be empty. The other way round (do not store a
-- branch in 'BranchSpec') does not work either because branches @a@ and @b@
-- might not actually be the branches in which the variables were consumed.
checkConsumedOverlap ::
  (Error.BranchSpec a, Error.BranchSpec b, MonadValidate Errors m) =>
  TcNameMap Values BranchConsumed ->
  a ->
  TcNameMap Values BranchConsumed ->
  b ->
  m ()
checkConsumedOverlap m1 other1 m2 other2 = Error.adds errors
  where
    errors =
      errs m1 m2 other2 ++ errs m2 m1 other1
    errs x y other =
      [ Error.branchedConsumeDifference name var consume loc other
      | (name, BranchConsumed consume var loc) <- Map.toList (x `Map.difference` y)
      ]

litType :: Pos -> E.Lit -> TcType
litType p = \case
  E.Unit -> T.Unit p
  E.Int _ -> builtinRef typeInt
  E.Char _ -> builtinRef typeChar
  E.String _ -> builtinRef typeString
  where
    builtinRef name =
      T.Type @Tc
        TypeRef
          { typeRefPos = p,
            typeRefName = name,
            typeRefArgs = [],
            typeRefKind = K.TU
          }

-- | Synthesizes the 'T.Arrow' type of a @"AlgST.Syntax.Expression".'Bind'@
-- (/E-LinAbs/ or /E-UnAbs/).
tysynthBind :: Pos -> E.Bind Rn -> TypeM (E.Bind Tc, TcType)
tysynthBind absLoc (E.Bind p _ v Nothing _) = do
  Error.fatal $ Error.synthUntypedLambda absLoc p v
tysynthBind absLoc (E.Bind p m v (Just ty) e) = do
  ty' <- kicheck ty K.TL
  (e', eTy) <- withProgVarBind (unrestrictedLoc absLoc m) p v ty' (tysynth e)

  -- Construct the resulting type.
  let funTy = T.Arrow absLoc m ty' eTy
  pure (E.Bind p m v (Just ty') e', funTy)

tycheckBind :: Pos -> E.Bind Rn -> TcType -> TypeM (E.Bind Tc)
tycheckBind absLoc bind@(E.Bind p m v mVarTy e) bindTy =
  case appArrow bindTy of
    Just (t, u) -> do
      -- Check the type annotation's kind.
      mVarTy' <- traverse (`kicheck` K.TL) mVarTy
      -- Give the bound variable the type from the annoation if available and
      -- fall back to the arrows domain.
      let varTy = t `fromMaybe` mVarTy'
      -- Check that the arrow type constructed from `varTy` and the context
      -- type's codomain is a subtype of the context type.
      --
      -- This checkes that the linearities of the arrows are well behaved
      -- relative to each other.
      requireSubtype absExpr (T.Arrow p m varTy u) bindTy
      -- Check the binding's body.
      e' <- withProgVarBind (unrestrictedLoc absLoc m) p v varTy (tycheck e u)
      pure $ E.Bind p m v (Just varTy) e'
    Nothing -> do
      Error.fatal $ Error.noArrowType absExpr bindTy
  where
    absExpr = E.Abs p bind

-- | Synthesizes the 'T.Forall' type of a @"AlgST.Syntax.Kind".'K.Bind' a@. The
-- type of @a@ is synthesized with the provided function.
tysynthTyBind ::
  (a -> TypeM (b, TcType)) ->
  Pos ->
  K.Bind TcStage a ->
  TypeM (K.Bind TcStage b, TcType)
tysynthTyBind synth p (K.Bind p' v k a) = do
  (a', t) <- local (bindTyVar v k) (synth a)
  let allT = T.Forall p (K.Bind p' v k t)
  pure (K.Bind p' v k a', allT)

tycheckTyBind ::
  (HasKiEnv env) =>
  (a -> TcType -> TcM env st b) ->
  RnExp ->
  K.Bind TcStage a ->
  TcType ->
  TcM env st (K.Bind TcStage b)
tycheckTyBind check bindExpr b@(K.Bind p v k a) t =
  case appTArrow t of
    Just (K.Bind p' v' k' t') | k == k' -> do
      let substMap = Map.singleton v' $ T.Var (p' @- k') v
      let tSubst = substituteType @Tc substMap t'
      local (bindTyVar v k) do
        K.Bind p v k <$> check a tSubst
    _ -> do
      Error.fatal $ Error.typeMismatchBind b t bindExpr

checkRecLam :: E.RecLam Rn -> TcType -> TypeM (E.RecLam Tc)
checkRecLam = go
  where
    go (E.RecTermAbs p b) =
      fmap (E.RecTermAbs p) . tycheckBind p b
    go abs@(E.RecTypeAbs p b) =
      fmap (E.RecTypeAbs p) . tycheckTyBind go (E.RecAbs abs) b

unrestrictedLoc :: (HasPos a) => a -> K.Multiplicity -> Maybe Pos
unrestrictedLoc p K.Un = Just $! pos p
unrestrictedLoc _ K.Lin = Nothing

freshLocal :: String -> TcM env st (TcName scope)
freshLocal = liftFresh . freshResolved . Name (ModuleName "") . Unqualified

-- | Establishes a set of bindings for a nested scope.
--
-- If the nested scope establishes an unrestricted context (such as an
-- unrestricted lambda expression @\(x:T) -> ...@) this function will verify
-- that no linear variables are consumed while checking the nested scope. For
-- this checking to happen the first argument must point to the location which
-- introduces the /unrestricted/ context. 'unrestrictedLoc' can be used as a
-- helper function.
withProgVarBinds ::
  Maybe Pos -> [(Located (ProgVar TcStage), TcType)] -> TypeM a -> TypeM a
withProgVarBinds !mUnArrLoc vtys action = etaTcM do
  let mkVar (p :@ v, ty) = etaTcM do
        let ki = typeKind ty
        usage <- case K.multiplicity ki of
          Just K.Lin
            | isWild v -> UnUsage <$ Error.add (Error.linearWildcard p ty)
            | otherwise -> pure LinUnunsed
          Just K.Un -> pure UnUsage
          Nothing -> UnUsage <$ Error.add (Error.unexpectedKind ty ki [K.TL])
        let var =
              Var
                { varUsage = usage,
                  varType = ty,
                  varLocation = p
                }
        pure (v, var)

  outerVars <- gets tcTypeEnv
  newVars <- Map.fromList <$> traverse mkVar vtys
  tcTypeEnvL <>= newVars
  a <- action
  resultVars <- gets tcTypeEnv

  whenJust mUnArrLoc \arrLoc -> do
    -- When in an unrestricted context check that no linear variables were
    -- consumed.
    Error.adds
      [ Error.invalidConsumed arrLoc name var usageLoc
      | (name, (var, usageLoc)) <- Map.toList (filterAdditionalConsumed outerVars resultVars)
      ]

  -- Emit an error for any of the new variables which are still 'UnusedLin'.
  Error.adds
    [ Error.missingUse name var
    | (name, var@Var {varUsage = LinUnunsed}) <-
        Map.toList (resultVars `Map.intersection` newVars)
    ]

  -- Continue with the variables as returned from the inner computation because
  -- a linear context may have consumed linear variables from the outer scope.
  tcTypeEnvL .= resultVars `Map.difference` newVars
  pure a

-- | Like 'withProgVarBinds' but for a single binding.
withProgVarBind ::
  Maybe Pos -> Pos -> ProgVar TcStage -> TcType -> TypeM a -> TypeM a
withProgVarBind mp varLoc pv ty = withProgVarBinds mp [(varLoc :@ pv, ty)]

tycheck :: RnExp -> TcType -> TypeM TcExp
tycheck e u = case e of
  --
  E.Abs p bnd -> do
    bnd' <- tycheckBind p bnd u
    pure (E.Abs p bnd')

  --
  E.TypeAbs p bnd -> do
    bnd' <- tycheckTyBind tycheck e bnd u
    pure (E.TypeAbs p bnd')

  --
  E.App p e1 e2 -> do
    (e2', t2) <- tysynth e2
    e1' <- tycheck e1 (T.Arrow p K.Lin t2 u)
    pure (E.App p e1' e2')

  --
  E.Pair p e1 e2 | T.Pair _ t1 t2 <- u -> do
    e1' <- tycheck e1 t1
    e2' <- tycheck e2 t2
    pure (E.Pair p e1' e2')

  --
  E.UnLet p v mty e body | bodyTy <- u -> do
    mty' <- maybe (pure Nothing) (\ty -> Just <$> kicheck ty K.TL) mty
    (e', ty') <- checkOrSynth e mty'
    withProgVarBind Nothing p v ty' do
      body' <- tycheck body bodyTy
      let branch =
            E.CaseBranch
              { branchPos = p,
                branchBinds = Identity (p :@ v),
                branchExp = body'
              }
      let caseMap =
            E.CaseMap
              { E.casesWildcard = Just branch,
                E.casesPatterns = Map.empty
              }
      pure (E.Exp $ ValueCase p e' caseMap)

  --
  E.PatLet p c vs e body | bodyTy <- u -> do
    (e', eTy) <- tysynth e
    pat <- extractMatchableType "Pattern let expression" (pos e) eTy
    let branch =
          E.CaseBranch
            { branchPos = pos c,
              branchExp = body,
              branchBinds = vs
            }
    let cases =
          E.CaseMap
            { casesPatterns = Map.singleton (unL c) branch,
              casesWildcard = Nothing
            }
    fst <$> checkPatternExpr p e' cases pat (Just bodyTy)

  --
  E.Case p e cases | branchTy <- u -> do
    (e', eTy) <- tysynth e
    pat <- extractMatchableType "Case expression scrutinee" (pos e) eTy
    fst <$> checkPatternExpr p e' cases pat (Just branchTy)

  --
  E.Cond p e eThen eElse -> do
    fst <$> checkIfExpr p e eThen eElse (Just u)

  -- fallback
  _ -> do
    (e', t) <- tysynth e
    requireSubtype e t u
    pure e'

requireSubtype :: (MonadValidate Errors m) => RnExp -> TcType -> TcType -> m ()
requireSubtype e t1 t2 = do
  nf1 <- normalize t1
  nf2 <- normalize t2
  when (Alpha nf1 </=? Alpha nf2) do
    Error.add (Error.typeMismatch e t1 nf1 t2 nf2)

-- | Returns the normalform of the given type or throws an error at the given
-- position.
normalize :: (MonadValidate Errors m) => TcType -> m TcType
normalize t = case nf t of
  Just t' -> pure t'
  Nothing -> Error.fatal (Error.noNormalform t)

bindTyVar :: (HasKiEnv env) => TypeVar TcStage -> K.Kind -> env -> env
bindTyVar !v !k = bindTyVars $ Identity (v, k)

bindTyVars :: (HasKiEnv env, Foldable f) => f (TypeVar TcStage, K.Kind) -> env -> env
bindTyVars vars = kiEnvL . tcKindEnvL <>~ varMap
  where
    varMap = foldl' (\m (v, k) -> Map.insert v k m) Map.empty vars

bindParams :: (HasKiEnv env) => Params TcStage -> env -> env
bindParams = bindTyVars . fmap (first unL)

errorIf :: (MonadValidate Errors m) => Bool -> Diagnostic -> m ()
errorIf True e = Error.add e
errorIf _ _ = pure ()

-- | @expectSubkind t k ks@ verifies that @k@ is a subkind of any of the kinds
-- @ks@. If not it errors and blames @t@ for the wrong kind @k@.
expectSubkind :: (MonadValidate Errors m) => RnType -> K.Kind -> [K.Kind] -> m ()
expectSubkind t actualKind allowedKinds =
  errorIf
    (not $ any (actualKind <=?) allowedKinds)
    (Error.unexpectedKind t actualKind allowedKinds)
