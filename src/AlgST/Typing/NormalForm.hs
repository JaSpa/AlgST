{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module AlgST.Typing.NormalForm (nf) where

import AlgST.Syntax.Kind qualified as K
import AlgST.Syntax.Name
import AlgST.Syntax.Traversal
import AlgST.Syntax.Type
import AlgST.Typing.Phase
import AlgST.Util.SourceLocation
import Control.Category ((<<<), (>>>))
import Data.Functor.Compose
import Data.Set qualified as Set
import Data.Tuple
import Data.Void

-- | Calcuates the positive normal form.
nf :: TcType -> Maybe TcType
nf = fst . nfs

-- | @nfs a@ calculates @(nf⁺(a), nf⁻(a))@.
nfs :: TcType -> (Maybe TcType, Maybe TcType)
nfs = go
  where
    wrap = Compose <<< Two
    unwrap = getCompose >>> getTwo

    negate :: XNegate Tc -> TcType -> (Maybe TcType, Maybe TcType)
    negate _ (Negate _ (Negate x t)) = negate x t
    negate _ (Negate _ t) = go t
    negate x t = unwrap $ Negate x <$> wrap (go t)

    polarized :: Polarity -> TcType -> (Polarity, TcType)
    polarized !p (Negate _ t) = polarized (flipPolarity p) t
    polarized !p t = (p, t)

    go :: TcType -> (Maybe TcType, Maybe TcType)
    go = \case
      Unit k ->
        (Just (Unit k), Nothing)
      Con x _ _ ->
        absurd x
      App x _ _ ->
        absurd x
      Var k v ->
        (Just (Var k v), Just $ Dualof (getRange k) (Var k v))
      Pair k t u ->
        let (t', _) = go t
            (u', _) = go u
         in (Pair k <$> t' <*> u', Nothing)
      Arrow k m a b ->
        let (a', _) = go a
            (b', _) = go b
         in (Arrow k m <$> a' <*> b', Nothing)
      End x p ->
        (Just (End x p), Just (End x (flipPolarity p)))
      Session k p a b ->
        let !(a', _) = go a
            !(b1, b2) = go b
            pa1 = polarized p <$> a'
            pa2 = polarized (flipPolarity p) <$> a'
         in (uncurry (Session k) <$> pa1 <*> b1, uncurry (Session k) <$> pa2 <*> b2)
      Dualof _ t' ->
        swap $ go t'
      Negate x t ->
        negate x t
      Forall tyK (K.Bind p' v k t)
        | (prepend', vs, Arrow arrK m t u) <- collectForalls t,
          not (liftNameSet (Set.insert v vs) `anyFree` t) ->
            let (t1, _) = go t
                (u1, _) = go (prepend (prepend' u))
             in (Arrow arrK m <$> t1 <*> u1, Nothing)
        | otherwise ->
            unwrap $ prepend <$> wrap (go t)
        where
          prepend :: TcType -> TcType
          prepend = Forall tyK . K.Bind p' v k
      Type ref ->
        let args = traverse (fst . go) (typeRefArgs ref)
            setArgs as = ref {typeRefArgs = as}
         in (Type . setArgs <$> args, Nothing)

{-
Potential for optimization
~~~~~~~~~~~~~~~~~~~~~~~~~~

\* A consecutive set of foralls is destructured and reconstrucuted multiple
  times during NF calculation.

\* Instead of pushing foralls down as far as possible we could gather them at
  the top level, eliminating the need for `anyFree` checks.

\* Check and probably improve strictness of the `nf` algorithm.
-}

collectForalls :: TcType -> (TcType -> TcType, TcNameSet Types, TcType)
collectForalls = go id mempty
  where
    go f vs = \case
      (Forall tyK (K.Bind p v k t) :: TcType) ->
        go (f . Forall @Tc tyK . K.Bind p v k) (Set.insert v vs) t
      t ->
        (f, vs, t)
