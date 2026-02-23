{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module AlgST.Typing.Align (Alpha (..)) where

import AlgST.Parse.Unparser ()
import AlgST.Syntax.Kind qualified as K
import AlgST.Syntax.Name
import AlgST.Syntax.Phases
import AlgST.Syntax.Type qualified as T
import AlgST.Typing.Phase
import AlgST.Util.PartialOrd
import Control.Applicative
import Control.Monad
import Data.Coerce
import Data.Hashable
import Data.Map.Strict qualified as Map
import Data.Monoid

newtype Alpha = Alpha TcType

instance Eq Alpha where
  (==) = coerce $ runAlign @CheckEquality

instance PartialOrd Alpha where
  (<=?) = coerce $ runAlign @CheckSubtype

newtype CheckEquality a = CheckEquality {runEquality :: Bool}
  deriving (Functor, Applicative) via (Const All)

instance TypeAlign CheckEquality where
  mergeAnnot :: AnnotKind a -> a -> a -> CheckEquality a
  mergeAnnot _ _ _ = CheckEquality True

  recordMismatch :: Mismatch a -> a -> a -> CheckEquality a
  recordMismatch _ _ _ = CheckEquality False

newtype CheckSubtype a = CheckSubtype Bool
  deriving (Functor, Applicative) via (Const All)

instance TypeAlign CheckSubtype where
  mergeAnnot :: AnnotKind a -> a -> a -> CheckSubtype a
  mergeAnnot _ _ _ = CheckSubtype True

  -- To enforce invariance we check types type ref args using equality.
  typeRefAlign :: (forall g. TypeAlign g => g a) -> CheckSubtype a
  typeRefAlign m = CheckSubtype $ runEquality m

  recordMismatch :: Mismatch a -> a -> a -> CheckSubtype a
  recordMismatch MultMismatch m1 m2 = CheckSubtype (m1 <= m2)
  recordMismatch _ _ _ = CheckSubtype False

data AlphaMap = AlphaMap !Word !(NameMapG TcStage Types AlphaVar)

newtype AlphaVar = AV Word
  deriving stock (Show)
  deriving newtype (Eq, Ord, Hashable)

runAlign :: TypeAlign f => TcType -> TcType -> f TcType
runAlign = alignTypes (AlphaMap 0 Map.empty)

alphaInsert :: [XTypeVar Tc] -> AlphaMap -> AlphaMap
alphaInsert vs (AlphaMap w m) =
  AlphaMap (w + 1) (foldl' (\m' v -> Map.insert v (AV w) m') m vs)

alphaLookup :: XTypeVar Tc -> AlphaMap -> Maybe AlphaVar
alphaLookup v (AlphaMap _ m) = Map.lookup v m

{- ORMOLU_DISABLE -}
data AnnotKind a where
  AnnotVar     :: AnnotKind (T.XVar Tc, XTypeVar Tc)
  AnnotUnit    :: AnnotKind (T.XUnit Tc)
  AnnotPair    :: AnnotKind (T.XPair Tc)
  AnnotArrow   :: AnnotKind (T.XArrow Tc)
  AnnotForall  :: AnnotKind (T.XForall Tc)
  AnnotBind    :: AnnotKind Pos
  AnnotSession :: AnnotKind (T.XSession Tc)
  AnnotEnd     :: AnnotKind (T.XEnd Tc)
  AnnotDualof  :: AnnotKind (T.XDualof Tc)
  AnnotNegate  :: AnnotKind (T.XNegate Tc)
{- ORMOLU_ENABLE -}

{- ORMOLU_DISABLE -}
data Mismatch a where
  -- | A not further specified structural mismatch of types.
  TypeMismatch     :: Mismatch TcType
  -- | A mismatch between the multiplicities of two arrows.
  MultMismatch     :: Mismatch K.Multiplicity
  -- | A mismatch between the kinds of variables bound in a 'T.Forall'.
  KindMismatch     :: Mismatch K.Kind
  -- | A mismatch between either the polarity of two @End!@/@End?@ types or of
  -- two 'T.Session' types.
  PolarityMismatch :: Mismatch T.Polarity
{- ORMOLU_ENABLE -}

data VarInfo = VarInfo
  { varName :: !(XTypeVar Tc),
    varAnnot :: !(T.XVar Tc),
    varAlpha :: !AlphaVar
  }

varAsType :: VarInfo -> TcType
varAsType = T.Var <$> varAnnot <*> varName

class Applicative f => TypeAlign f where
  mergeAnnot :: AnnotKind a -> a -> a -> f a
  recordMismatch :: Mismatch a -> a -> a -> f a

  typeRefAlign :: (forall g. TypeAlign g => g a) -> f a
  typeRefAlign f = f

  alignVarL :: VarInfo -> TcType -> f TcType
  alignVarL = recordMismatch TypeMismatch . varAsType

  alignVarR :: TcType -> VarInfo -> f TcType
  alignVarR t = recordMismatch TypeMismatch t . varAsType

  alignVars :: VarInfo -> VarInfo -> f TcType
  alignVars v1 v2
    | varAlpha v1 == varAlpha v2 =
        uncurry T.Var <$> mergeAnnot AnnotVar a1 a2
    | otherwise =
        recordMismatch TypeMismatch t1 t2
    where
      a1 = (varAnnot v1, varName v1)
      a2 = (varAnnot v2, varName v2)
      t1 = varAsType v1
      t2 = varAsType v2

type AlignFn f a = AlphaMap -> a -> a -> f a

alignTypes :: forall f. TypeAlign f => AlignFn f TcType
alignTypes = go
  where
    go :: AlignFn f TcType
    go !m t1 t2
      | Just v1 <- av1, Just v2 <- av2 = alignVars v1 v2
      | Just v1 <- av1 = alignVarL v1 t2
      | Just v2 <- av2 = alignVarR t1 v2
      where
        av1 = getAlphaVar m t1
        av2 = getAlphaVar m t2
    -- Neither variable was quantified inside the aligned type.
    go !_ (T.Var x1 v1) (T.Var x2 v2)
      | v1 == v2 = uncurry T.Var <$> mergeAnnot AnnotVar (x1, v1) (x2, v2)
    go !_ (T.Unit x1) (T.Unit x2) =
      T.Unit
        <$> mergeAnnot AnnotUnit x1 x2
    go !m (T.Pair x1 t1 u1) (T.Pair x2 t2 u2) =
      T.Pair
        <$> mergeAnnot AnnotPair x1 x2
        <*> go m t1 t2
        <*> go m u1 u2
    go !m (T.Arrow x1 m1 t1 u1) (T.Arrow x2 m2 t2 u2) =
      T.Arrow
        <$> mergeAnnot AnnotArrow x1 x2
        <*> checkMismatch MultMismatch m1 m2
        <*> go m t2 t1
        <*> go m u1 u2
    go !m (T.Forall x1 (K.Bind p1 v1 k1 t1)) (T.Forall x2 (K.Bind p2 v2 k2 t2)) =
      (\x x' v k -> T.Forall x . K.Bind x' v k)
        <$> mergeAnnot AnnotForall x1 x2
        <*> mergeAnnot AnnotBind p1 p2
        <*> pure v1
        <*> checkMismatch KindMismatch k1 k2
        <*> go (alphaInsert [v1, v2] m) t1 t2
    go !m (T.Session x1 p1 t1 u1) (T.Session x2 p2 t2 u2) =
      T.Session
        <$> mergeAnnot AnnotSession x1 x2
        <*> checkMismatch PolarityMismatch p1 p2
        <*> t
        <*> go m u1 u2
      where
        -- Change the sides for correct subtype checks in session types:
        -- If our side has to provide a t₂, and t₁ <: t₂, then giving a t₁
        -- is fine. However when receiving, it is ok to expect a larger type
        -- than there is actually coming.
        t = case p1 of
          T.In -> go m t1 t2
          T.Out -> go m t2 t1
    go !_ (T.End x1 p1) (T.End x2 p2) =
      T.End
        <$> mergeAnnot AnnotEnd x1 x2
        <*> checkMismatch PolarityMismatch p1 p2
    go !m (T.Dualof x1 t1) (T.Dualof x2 t2) =
      T.Dualof
        <$> mergeAnnot AnnotDualof x1 x2
        <*> go m t1 t2
    go !m (T.Negate x1 t1) (T.Negate x2 t2) =
      T.Negate
        <$> mergeAnnot AnnotNegate x1 x2
        <*> go m t1 t2
    go !m (T.Type r1) (T.Type r2)
      | typeRefName r1 == typeRefName r2 && typeRefKind r1 == typeRefKind r2 = do
          args <-
            zipWithM
              (\t u -> typeRefAlign $ alignTypes m t u)
              (typeRefArgs r1)
              (typeRefArgs r2)
          pure $ T.Type r1 {typeRefArgs = args}
    go !_ t1 t2 =
      recordMismatch TypeMismatch t1 t2

    checkMismatch :: Eq a => Mismatch a -> a -> a -> f a
    checkMismatch m x y
      | x == y = pure x
      | otherwise = recordMismatch m x y

getAlphaVar :: AlphaMap -> TcType -> Maybe VarInfo
getAlphaVar m t = do
  T.Var x v <- pure t
  av <- alphaLookup v m
  pure VarInfo {varName = v, varAnnot = x, varAlpha = av}
