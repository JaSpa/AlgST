{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

-- | This module defines a datatype @'OncePair' a b@ isomorphic to @(a,b)@.
-- @'OncePair' a b@ is optimised for the case that in each branch at most one
-- component is evaluated.
--
-- When constructing values of this type the compiler understands that only one
-- component will be evaluated. It can reduce allocations by not sharing
-- subcomputations between the two branches. Instead all subcomputations are
-- inlined into the branches. Therefore, if against all advice both components
-- of a 'OncePair' are evaluated some computations which would otherwise be
-- shared will have to be recomputed.
module AlgST.Util.OncePair
  ( OncePair ((:|:)),
    onceFst,
    onceSnd,
    onceSwap,
  )
where

import Data.Bifunctor
import Data.Semigroup
import GHC.Exts (oneShot)

data Sel a b r where
  Fst :: Sel a b a
  Snd :: Sel a b b

newtype OncePair a b = OncePair (forall r. Sel a b r -> r)

instance Functor (OncePair a) where
  fmap = second

instance Bifunctor OncePair where
  bimap f g (a :|: b) = f a :|: g b

instance (Semigroup a, Semigroup b) => Semigroup (OncePair a b) where
  (a1 :|: b1) <> (a2 :|: b2) = a1 <> a2 :|: b1 <> b2
  stimes n (a :|: b) = stimes n a :|: stimes n b

instance (Monoid a, Monoid b) => Monoid (OncePair a b) where
  mempty = mempty :|: mempty

infix 2 :|:

pattern (:|:) :: a -> b -> OncePair a b
pattern a :|: b <- ((,) <$> onceFst <*> onceSnd -> (a, b))
  where
    a :|: b = OncePair $ oneShot \case
      Fst -> a
      Snd -> b

{-# COMPLETE (:|:) #-}

onceFst :: OncePair a b -> a
onceFst (OncePair f) = f Fst

onceSnd :: OncePair a b -> b
onceSnd (OncePair f) = f Snd

onceSwap :: OncePair a b -> OncePair b a
onceSwap = (:|:) <$> onceSnd <*> onceFst
