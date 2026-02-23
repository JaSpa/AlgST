{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- | This module defines an extensible way to traverse the variables in syntax
-- constructs. It is based upon the ideas described in
-- [Writing efficient free variable traversals](https://www.haskell.org/ghc/blog/20190728-free-variable-traversals.html).
module AlgST.Syntax.Traversal
  ( -- * Classes
    SynTraversal (..),
    SynTraversable (..),
    traverseSyntaxIn,
    traverseSyntaxBetween,
    bindOne,

    -- * Predefined Traversals

    -- ** @CollectFree@
    CollectFree (..),
    collectFree,

    -- ** @Any@
    Any (..),
    anyFree,

    -- ** Subsitution
    substituteType,
    substituteTerm,

    -- *** Keeping substitutions around.
    Substitutions (..),
    applySubstitutions,
    nullSubstitutions,
    emptySubstitutions,
    typeSubstitions,
    termSubstitions,

    -- * Traversal helpers
    Two (..),
    Pxy,
    mkPxy,
  )
where

import AlgST.Syntax.Expression qualified as E
import AlgST.Syntax.Kind qualified as K
import AlgST.Syntax.Name
import AlgST.Syntax.Operators
import AlgST.Syntax.Phases
import AlgST.Syntax.Type qualified as T
import Control.Applicative
import Control.Category ((>>>))
import Control.Monad.Eta
import Control.Monad.Trans.Reader
import Data.Bitraversable
import Data.Functor.Classes
import Data.Functor.Compose
import Data.Functor.Identity
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Monoid qualified as M
import Data.Set qualified as Set
import Data.Singletons
import Data.Void
import GHC.Exts (Proxy#, proxy#)

type Pxy x y = Proxy# (x, y)

mkPxy :: () -> forall x y. Pxy x y
mkPxy _ = proxy#

class Applicative f => SynTraversal f x y where
  valueVariable :: Pxy x y -> E.XVar x -> XName x Values -> f (E.Exp y)
  typeVariable :: Pxy x y -> T.XVar x -> XName x Types -> f (T.Type y)

  -- | Lift a type/value constructor from stage @x@ to stage @y@.
  --
  -- To gurantee invariants assumed by 'traverseNameMap' no two constructor
  -- names which compare different in stage @x@ may be turned into names which
  -- compare equal in stage @y@.
  useConstructor ::
    SingI scope =>
    Pxy x y ->
    Pos ->
    XName x scope ->
    f (XName y scope)

  -- | Binds a set of variables for the given computation.
  bind ::
    (Traversable t, SingI s) =>
    Pxy x y ->
    t (XName x s) ->
    (t (XName y s) -> f a) ->
    f a

  exprExtension :: Pxy x y -> E.XExp x -> f (E.Exp y)
  typeExtension :: Pxy x y -> T.XType x -> f (T.Type y)

bindOne ::
  forall x s f a y.
  (SynTraversal f x y, SingI s) =>
  Pxy x y ->
  XName x s ->
  (XName y s -> f a) ->
  f a
bindOne pxy v f = bind pxy (Identity v) (f . runIdentity)

newtype CollectFree stage = CollectFree
  { runCollect ::
      -- Bound variables
      ANameSetG stage ->
      -- Accumulator
      ANameSetG stage ->
      -- Result
      ANameSetG stage
  }

instance Semigroup (CollectFree stage) where
  fv1 <> fv = CollectFree $ oneShot \bound -> oneShot \acc ->
    runCollect fv1 bound (runCollect fv bound acc)

instance Monoid (CollectFree stage) where
  mempty = CollectFree (const id)

instance
  ( x ~ y,
    XStage x ~ stage,
    SynTraversable (E.XExp x) x (E.XExp x) x,
    SynTraversable (T.XType x) x (T.XType x) x
  ) =>
  SynTraversal (Const (CollectFree stage)) x y
  where
  typeVariable _ _ = varCollectFree
  valueVariable _ _ = varCollectFree
  useConstructor _ _ = pure

  bind _ vs f = Const . CollectFree $ oneShot \bound -> oneShot \acc ->
    let !bound' = foldl' (flip $ Set.insert . liftName) bound vs
     in runCollect (getConst (f vs)) bound' acc

  exprExtension pxy = fmap E.Exp . traverseSyntax pxy
  typeExtension pxy = fmap T.Type . traverseSyntax pxy

-- Inserts the given variable into the set of free variables.
varCollectFree :: SingI scope => Name stage scope -> Const (CollectFree stage) b
varCollectFree (liftName -> v) = Const . CollectFree $ oneShot \bound -> oneShot \acc ->
  -- Don't include it if it is either bound or already recorded.
  if v `Set.member` bound || v `Set.member` acc
    then acc
    else Set.insert v acc

-- | Collects all free variables.
collectFree ::
  forall s x.
  ( SynTraversable (s x) x (s x) x,
    SynTraversable (E.XExp x) x (E.XExp x) x,
    SynTraversable (T.XType x) x (T.XType x) x
  ) =>
  s x ->
  ANameSetG (XStage x)
collectFree a = runCollect (getConst (traverseSyntaxIn a a)) mempty mempty

newtype Any stage = Any {runAny :: ANameSetG stage -> Bool}
  deriving (Monoid) via (ANameSetG stage -> M.Any)

instance Semigroup (Any stage) where
  fv1 <> fv2 = Any $ oneShot \intresting ->
    runAny fv1 intresting || runAny fv2 intresting

instance
  ( x ~ y,
    XStage x ~ stage,
    SynTraversable (E.XExp x) x (E.XExp x) x,
    SynTraversable (T.XType x) x (T.XType x) x
  ) =>
  SynTraversal (Const (Any stage)) x y
  where
  valueVariable _ _ = anyVariable
  typeVariable _ _ = anyVariable
  useConstructor _ _ = pure

  -- When a variable is bound it will be removed from the set of intresting
  -- variables. If the set becomes empty we will short-circuit to False.
  bind _ vs f = Const . Any $ oneShot \intresting -> do
    let intresting' = foldl' (flip (Set.delete . liftName)) intresting vs
     in not (Set.null intresting') && runAny (getConst (f vs)) intresting'

  exprExtension pxy = fmap E.Exp . traverseSyntax pxy
  typeExtension pxy = fmap T.Type . traverseSyntax pxy

anyVariable :: SingI scope => Name stage scope -> Const (Any stage) a
anyVariable = Const . Any . Set.member . liftName

-- | Checks if any of the variables in the given set are free in the given
-- piece of syntax.
anyFree ::
  forall s x.
  ( SynTraversable (s x) x (s x) x,
    SynTraversable (E.XExp x) x (E.XExp x) x,
    SynTraversable (T.XType x) x (T.XType x) x
  ) =>
  ANameSetG (XStage x) ->
  s x ->
  Bool
anyFree vars a
  | Set.null vars = False
  | otherwise = runAny (getConst (traverseSyntaxIn a a)) vars

data Substitutions x = Substitutions
  { progVarSubs :: !(NameMapG (XStage x) Values (E.Exp x)),
    typeVarSubs :: !(NameMapG (XStage x) Types (T.Type x))
  }

instance
  ( HasPos (E.XCon x),
    HasPos (T.XCon x),
    E.SameX x y,
    T.SameX x y,
    XStage x ~ XStage y
  ) =>
  SynTraversable (Substitutions x) x (Substitutions y) y
  where
  traverseSyntax pxy subs =
    Substitutions
      <$> traverse (traverseSyntax pxy) (progVarSubs subs)
      <*> traverse (traverseSyntax pxy) (typeVarSubs subs)

emptySubstitutions :: Substitutions x
emptySubstitutions = Substitutions Map.empty Map.empty

nullSubstitutions :: Substitutions x -> Bool
nullSubstitutions = (&&) <$> Map.null . progVarSubs <*> Map.null . typeVarSubs

typeSubstitions :: NameMapG (XStage x) Types (T.Type x) -> Substitutions x
typeSubstitions s = emptySubstitutions {typeVarSubs = s}

termSubstitions :: NameMapG (XStage x) Values (E.Exp x) -> Substitutions x
termSubstitions s = emptySubstitutions {progVarSubs = s}

instance
  ( x ~ y,
    SynTraversable (E.XExp x) x (E.XExp x) x,
    SynTraversable (T.XType x) x (T.XType x) x,
    Monad m
  ) =>
  SynTraversal (ReaderT (Substitutions x) m) x y
  where
  valueVariable _ x = etaReaderT . asks . subVar (E.Var x) progVarSubs
  typeVariable _ x = etaReaderT . asks . subVar (T.Var x) typeVarSubs
  useConstructor _ _ = pure

  -- Remove the bound variables from the maps.
  bind _ vs f = etaReaderT do
    let removeVar subs =
          liftName >>> \case
            Left tv -> subs {typeVarSubs = Map.delete tv (typeVarSubs subs)}
            Right pv -> subs {progVarSubs = Map.delete pv (progVarSubs subs)}
    local (\s -> foldl' removeVar s vs) do
      f vs

  exprExtension pxy = fmap E.Exp . traverseSyntax pxy
  typeExtension pxy = fmap T.Type . traverseSyntax pxy

subVar ::
  (Name stage scope -> a) ->
  (Substitutions x -> NameMapG stage scope a) ->
  Name stage scope ->
  Substitutions x ->
  a
subVar def f v = fromMaybe (def v) . Map.lookup v . f

applySubstitutions ::
  forall a x.
  ( SynTraversable a x a x,
    SynTraversable (E.XExp x) x (E.XExp x) x,
    SynTraversable (T.XType x) x (T.XType x) x
  ) =>
  Substitutions x ->
  (a -> a)
applySubstitutions s a
  | nullSubstitutions s = a
  | otherwise = runReader (traverseSyntaxIn (Proxy @x) a) s

-- | Substitute a single 'TypeVar'.
substituteType ::
  forall x a.
  ( SynTraversable a x a x,
    SynTraversable (E.XExp x) x (E.XExp x) x,
    SynTraversable (T.XType x) x (T.XType x) x
  ) =>
  NameMapG (XStage x) Types (T.Type x) ->
  (a -> a)
substituteType = applySubstitutions . typeSubstitions

-- | Substitute a single 'ProgVar'.
substituteTerm ::
  forall x a.
  ( SynTraversable a x a x,
    SynTraversable (E.XExp x) x (E.XExp x) x,
    SynTraversable (T.XType x) x (T.XType x) x
  ) =>
  NameMapG (XStage x) Values (E.Exp x) ->
  (a -> a)
substituteTerm = applySubstitutions . termSubstitions

newtype Two a = Two {getTwo :: (a, a)}
  deriving stock (Functor, Foldable, Traversable)

instance Applicative Two where
  pure a = Two (a, a)
  Two (fa, fb) <*> Two (a, b) = Two (fa a, fb b)

instance Show1 Two where
  liftShowsPrec f g p (Two xy) =
    showParen (p > 10) $
      liftShowsPrec2 f g f g 11 xy

instance Show a => Show (Two a) where
  showsPrec = showsPrec1

class SynTraversable a x b y where
  traverseSyntax :: SynTraversal f x y => Pxy x y -> a -> f b

traverseSyntaxIn ::
  forall x a f proxy.
  (SynTraversable a x a x, SynTraversal f x x) =>
  (proxy x -> a -> f a)
traverseSyntaxIn (_ :: proxy x) = traverseSyntax (mkPxy () @x @x)

traverseSyntaxBetween ::
  forall x y a b f p1 p2.
  (SynTraversable a x b y, SynTraversal f x y) =>
  (p1 x -> p2 y -> a -> f b)
traverseSyntaxBetween (_ :: p1 x) (_ :: p2 y) = traverseSyntax (mkPxy () @x @y)

instance SynTraversable Void x a y where
  traverseSyntax _ = absurd

instance
  (SynTraversable a x a' y, SynTraversable b x b' y) =>
  SynTraversable (Either a b) x (Either a' b') y
  where
  traverseSyntax pxy = bitraverse (traverseSyntax pxy) (traverseSyntax pxy)

instance
  ( HasPos (E.XCon x),
    HasPos (T.XCon x),
    E.SameX x y,
    T.SameX x y
  ) =>
  SynTraversable (E.Exp x) x (E.Exp y) y
  where
  traverseSyntax pxy = \case
    E.Lit x l ->
      pure (E.Lit x l)
    E.Var x v ->
      valueVariable pxy x v
    E.Con x c ->
      E.Con x
        <$> useConstructor pxy (pos x) c
    E.Abs x b ->
      E.Abs x
        <$> traverseSyntax pxy b
    E.App x e1 e2 ->
      E.App x
        <$> traverseSyntax pxy e1
        <*> traverseSyntax pxy e2
    E.Pair x e1 e2 ->
      E.Pair x
        <$> traverseSyntax pxy e1
        <*> traverseSyntax pxy e2
    E.UnLet x v mty e k -> do
      let unLet mty' e' (v', k') =
            E.UnLet x v' mty' e' k'
      unLet
        <$> traverse (traverseSyntax pxy) mty
        <*> traverseSyntax pxy e
        <*> bindOne pxy v \v' -> (v',) <$> traverseSyntax pxy k
    E.PatLet x v vs e k -> do
      let patLet v' e' (vs', k') =
            E.PatLet x (v @- v') vs' e' k'
      patLet
        <$> uncurryL (useConstructor pxy) v
        <*> traverseSyntax pxy e
        <*> bind pxy (Compose vs) \(Compose vs') -> (vs',) <$> traverseSyntax pxy k
    E.Rec x v ty e -> do
      let expRec ty' (v', e') =
            E.Rec x v' ty' e'
      expRec
        <$> traverseSyntax pxy ty
        <*> bindOne pxy v \v' -> (v',) <$> traverseSyntax pxy e
    E.Cond x eCond eThen eElse ->
      E.Cond x
        <$> traverseSyntax pxy eCond
        <*> traverseSyntax pxy eThen
        <*> traverseSyntax pxy eElse
    E.Case x e cases ->
      E.Case x
        <$> traverseSyntax pxy e
        <*> traverseSyntax pxy cases
    E.TypeAbs x b ->
      E.TypeAbs x
        <$> traverseSyntax pxy b
    E.TypeApp x e t ->
      E.TypeApp x
        <$> traverseSyntax pxy e
        <*> traverseSyntax pxy t
    E.New x t ->
      E.New x
        <$> traverseSyntax pxy t
    E.Select x c -> do
      let select c' =
            E.Select x (c @- c')
      select
        <$> uncurryL (useConstructor pxy) c
    E.Fork x e ->
      E.Fork x
        <$> traverseSyntax pxy e
    E.Fork_ x e ->
      E.Fork_ x
        <$> traverseSyntax pxy e
    E.Exp x ->
      exprExtension pxy x

instance
  ( HasPos (E.XCon x),
    HasPos (T.XCon x),
    E.SameX x y,
    T.SameX x y
  ) =>
  SynTraversable (E.RecLam x) x (E.RecLam y) y
  where
  traverseSyntax pxy = \case
    E.RecTermAbs x b -> E.RecTermAbs x <$> traverseSyntax pxy b
    E.RecTypeAbs x b -> E.RecTypeAbs x <$> traverseSyntax pxy b

instance
  ( HasPos (E.XCon x),
    HasPos (T.XCon x),
    E.SameX x y,
    T.SameX x y,
    Traversable f,
    Traversable g
  ) =>
  SynTraversable (E.CaseMap' f g x) x (E.CaseMap' f g y) y
  where
  traverseSyntax pxy cm =
    E.CaseMap
      <$> traverseNameMap pxy (traverseSyntax pxy) (E.casesPatterns cm)
      <*> traverse (traverseSyntax pxy) (E.casesWildcard cm)

instance
  ( HasPos (E.XCon x),
    HasPos (T.XCon x),
    E.SameX x y,
    T.SameX x y,
    Traversable f
  ) =>
  SynTraversable (E.CaseBranch f x) x (E.CaseBranch f y) y
  where
  traverseSyntax pxy b =
    bind pxy (Compose (E.branchBinds b)) \(Compose binds) -> do
      e <- traverseSyntax pxy (E.branchExp b)
      pure
        b
          { E.branchBinds = binds,
            E.branchExp = e
          }

instance
  ( HasPos (E.XCon x),
    HasPos (T.XCon x),
    E.SameX x y,
    T.SameX x y
  ) =>
  SynTraversable (E.Bind x) x (E.Bind y) y
  where
  traverseSyntax pxy (E.Bind x m v t e) = do
    let mkBind t' (v', e') =
          E.Bind x m v' t' e'
    mkBind
      <$> traverse (traverseSyntax pxy) t
      <*> bindOne pxy v (\v' -> (v',) <$> traverseSyntax pxy e)

instance
  ( HasPos (E.XCon x),
    HasPos (T.XCon x),
    E.SameX x y,
    T.SameX x y
  ) =>
  SynTraversable (OperatorSequence x) x (OperatorSequence y) y
  where
  traverseSyntax pxy = \case
    -- The `rs` should not be traversed because they are references to the
    -- last element in `ne`.
    OperandFirst rs ne -> do
      ne' <- traverse (traverseSyntax pxy) ne
      pure $ OperandFirst (NE.last ne' <$ rs) ne'
    OperatorFirst rs ne -> do
      ne' <- traverse (traverseSyntax pxy) ne
      pure $ OperatorFirst (NE.last ne' <$ rs) ne'

instance
  ( HasPos (T.XCon x),
    T.SameX x y
  ) =>
  SynTraversable (T.Type x) x (T.Type y) y
  where
  traverseSyntax pxy = \case
    T.Unit x ->
      pure (T.Unit x)
    T.Arrow x m t u ->
      T.Arrow x m
        <$> traverseSyntax pxy t
        <*> traverseSyntax pxy u
    T.Pair x t u ->
      T.Pair x
        <$> traverseSyntax pxy t
        <*> traverseSyntax pxy u
    T.Session x p t u ->
      T.Session x p
        <$> traverseSyntax pxy t
        <*> traverseSyntax pxy u
    T.End x p ->
      pure $ T.End x p
    T.Forall x b ->
      T.Forall x
        <$> traverseSyntax pxy b
    T.Var x v ->
      typeVariable pxy x v
    T.Con x v ->
      T.Con x
        <$> useConstructor pxy (pos x) v
    T.App x t u ->
      T.App x
        <$> traverseSyntax pxy t
        <*> traverseSyntax pxy u
    T.Dualof x t ->
      T.Dualof x
        <$> traverseSyntax pxy t
    T.Negate x t ->
      T.Negate x
        <$> traverseSyntax pxy t
    T.Type x ->
      typeExtension pxy x

instance
  ( SynTraversable a x b y,
    XStage x ~ s,
    XStage y ~ t
  ) =>
  SynTraversable (K.Bind s a) x (K.Bind t b) y
  where
  traverseSyntax pxy (K.Bind p v k t) =
    bindOne pxy v \v' ->
      K.Bind p v' k <$> traverseSyntax pxy t

traverseNameMap ::
  (SynTraversal f x y, SingI scope, HasPos a) =>
  Pxy x y ->
  (a -> f b) ->
  NameMapG (XStage x) scope a ->
  f (NameMapG (XStage y) scope b)
traverseNameMap pxy f =
  -- We can't assume that 'useConstructor' is monotonic in respect to name
  -- ordering.
  Map.toAscList
    >>> traverse (\(n, a) -> (,) <$> useConstructor pxy (pos a) n <*> f a)
    >>> fmap Map.fromList
