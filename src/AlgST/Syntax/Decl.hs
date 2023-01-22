{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module AlgST.Syntax.Decl where

import AlgST.Syntax.Expression qualified as E
import AlgST.Syntax.Kind qualified as K
import AlgST.Syntax.Name
import AlgST.Syntax.Phases
import AlgST.Syntax.Type qualified as T
import AlgST.Util.SourceLocation (SrcRange)
import AlgST.Util.SourceLocation qualified as R
import Data.Functor.Identity
import Data.Kind qualified as Hs
import Data.Map.Strict qualified as Map
import Data.Maybe
import Language.Haskell.TH.Syntax (Lift)

{- ORMOLU_DISABLE -}
type family XAliasDecl x
type family XDataDecl x
type family XProtocolDecl x

type family XDataCon x
type family XProtoCon x
{- ORMOLU_ENABLE -}

type ForallDeclX (c :: Hs.Type -> Hs.Constraint) x =
  ( c (XAliasDecl x),
    c (XDataDecl x),
    c (XProtocolDecl x)
  )

type ForallConX (c :: Hs.Type -> Hs.Constraint) x =
  ( c (XDataCon x),
    c (XProtoCon x)
  )

type ForallX (c :: Hs.Type -> Hs.Constraint) x =
  ( ForallConX c x,
    ForallDeclX c x
  )

data TypeAlias x = TypeAlias
  { aliasParams :: XParams x,
    aliasKind :: Maybe K.Kind,
    aliasType :: T.Type x
  }

deriving stock instance (T.ForallX Lift x) => Lift (TypeAlias x)

data TypeNominal stage c = TypeNominal
  { nominalParams :: Params stage,
    nominalKind :: K.Kind,
    nominalConstructors :: RConstructors stage c
  }
  deriving (Lift)

type Params stage = [(Located (Name stage Types), K.Kind)]

type XParams x = Params (XStage x)

type Constructors stage a = NameMapG stage Values (Pos, [a])

type RConstructors stage a = NameMapG stage Values (SrcRange, [a])

mapConstructors ::
  (ProgVar stage -> a -> b) ->
  (Constructors stage a -> Constructors stage b)
mapConstructors f = runIdentity . traverseConstructors (\k -> Identity . f k)

traverseConstructors ::
  Applicative f =>
  (ProgVar stage -> a -> f b) ->
  (Constructors stage a -> f (Constructors stage b))
traverseConstructors f = Map.traverseWithKey (traverse . traverse . f)

data TypeDecl x
  = AliasDecl (XAliasDecl x) (TypeAlias x)
  | DataDecl (XDataDecl x) (TypeNominal (XStage x) (T.Type x))
  | ProtoDecl (XProtocolDecl x) (TypeNominal (XStage x) (T.Type x))

deriving stock instance (ForallDeclX Lift x, T.ForallX Lift x) => Lift (TypeDecl x)

declParams :: TypeDecl x -> XParams x
declParams = \case
  AliasDecl _ alias -> aliasParams alias
  DataDecl _ decl -> nominalParams decl
  ProtoDecl _ decl -> nominalParams decl

declConstructors ::
  forall x.
  (XDataCon x ~ Pos, XProtoCon x ~ Pos) =>
  XTypeVar x ->
  TypeDecl x ->
  NameMapG (XStage x) Values (ConstructorDecl x)
declConstructors name d = case d of
  AliasDecl _ _ ->
    Map.empty
  DataDecl _ decl -> do
    -- Falling back to 'Un' is not 100% correct but we know that for a well
    -- formed data declaration the kind *must* have multiplicity information.
    -- So the only way to get 'Nothing' here is when the user annotated the
    -- declaration to have kind 'P' which will lead to an erorr diagnosis.
    let mul = K.Un `fromMaybe` K.multiplicity (nominalKind decl)
    let con (r, items) = DataCon @x (R.needPos r) name (declParams d) mul items
    Map.map con (nominalConstructors decl)
  ProtoDecl _ decl -> do
    let con (r, items) = ProtocolCon @x (R.needPos r) name (declParams d) items
    Map.map con (nominalConstructors decl)

instance ForallDeclX HasPos x => HasPos (TypeDecl x) where
  pos = \case
    AliasDecl x _ -> pos x
    DataDecl x _ -> pos x
    ProtoDecl x _ -> pos x

data SignatureDecl x = SignatureDecl
  { signaturePos :: Pos,
    signatureType :: T.Type x
  }

deriving stock instance (T.ForallX Lift x) => Lift (SignatureDecl x)

instance HasPos (SignatureDecl x) where
  pos = signaturePos

data ValueDecl x = ValueDecl
  { valuePos :: Pos,
    valueType :: T.Type x,
    valueParams :: [Located (ANameG (XStage x))],
    valueBody :: E.Exp x
  }

deriving stock instance (E.ForallX Lift x, T.ForallX Lift x) => Lift (ValueDecl x)

instance HasPos (ValueDecl x) where
  pos = valuePos

valueSignatureDecl :: ValueDecl x -> SignatureDecl x
valueSignatureDecl = SignatureDecl <$> valuePos <*> valueType

data ConstructorDecl x
  = -- | A data constructor is annotated with
    --
    -- * the parent type's name
    -- * the parent type's parameters
    -- * the parent type's 'Multiplicity'
    -- * the constructor's items
    DataCon (XDataCon x) !(XTypeVar x) (XParams x) !K.Multiplicity [T.Type x]
  | -- | A protocol (non-data) constructor is annotated with
    --
    -- * the parent type's name
    -- * the parent type's parameters
    -- * the constructor's items
    ProtocolCon (XProtoCon x) !(XTypeVar x) (XParams x) [T.Type x]

deriving stock instance (ForallConX Lift x, T.ForallX Lift x) => Lift (ConstructorDecl x)

conParent :: ConstructorDecl x -> XTypeVar x
conParent = \case
  DataCon _ n _ _ _ -> n
  ProtocolCon _ n _ _ -> n

conParams :: ConstructorDecl x -> XParams x
conParams = \case
  DataCon _ _ ps _ _ -> ps
  ProtocolCon _ _ ps _ -> ps

conItems :: ConstructorDecl x -> [T.Type x]
conItems = \case
  DataCon _ _ _ _ ts -> ts
  ProtocolCon _ _ _ ts -> ts

instance ForallConX HasPos x => HasPos (ConstructorDecl x) where
  pos = \case
    DataCon x _ _ _ _ -> pos x
    ProtocolCon x _ _ _ -> pos x
