{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module AlgST.Syntax.Name
  ( -- * Type/Value Names
    Name (.., UnqualifiedName),

    -- ** Pretty-printing names
    pprName,
    pprResolved,

    -- ** Accessing name parts
    nameWritten,
    nameUnqualified,
    nameWrittenModule,
    nameResolvedModule,
    nameResolvedId,
    nameWrittenL,
    nameUnqualifiedL,
    nameWrittenModuleL,
    nameResolvedModuleL,
    nameResolvedIdL,

    -- ** Predefined names
    pattern Wildcard,
    isWild,
    pattern PairCon,

    -- ** Unqualified parts
    Unqualified (..),
    pattern MainFunction,

    -- ** Resolved Names
    ResolvedId,
    firstResolvedId,
    nextResolvedId,

    -- ** Abbreviations
    NameW,
    NameR,
    ProgVar,
    TypeVar,
    NameMap,
    NameSet,

    -- ** Unscoped Names
    AName,
    ANameMap,
    ANameSet,
    ANameLike (..),
    liftName,
    liftNameSet,
    liftNameMap,
    eitherName,

    -- ** Stage-generic Abbreviations
    ANameG,
    ANameMapG,
    ANameSetG,
    NameMapG,
    NameSetG,

    -- * Modules
    ModuleName (..),
    pattern MainModule,
    emptyModuleName,
    unModuleName,
    modulePath,
    moduleFromPath,

    -- * Scopes
    Scope (..),
    ScopeIndexed (..),
    scopeL,
    scopeL',
    ScopedVariants,
    scopedVariants,

    -- * Type Level Tags
    SScope (..),
    TypesSym0,
    ValuesSym0,
    Stage (..),
    WrittenSym0,
    ResolvedSym0,
  )
where

import Control.Category ((>>>))
import Control.Monad
import Data.Foldable
import Data.Hashable
import Data.Kind
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Singletons.TH
import GHC.Generics (Generic)
import Language.Haskell.TH.Syntax (Lift)
import Lens.Family2 qualified as L
import Lens.Family2.Stock qualified as L
import Lens.Family2.Unchecked qualified as L
import System.FilePath qualified as FP

-- | The name of a module.
newtype ModuleName = ModuleName String
  deriving stock (Show, Lift)
  deriving newtype (Eq, Ord, Hashable)

pattern MainModule :: ModuleName
pattern MainModule = ModuleName "Main"

pattern MainFunction :: Unqualified
pattern MainFunction = Unqualified "main"

emptyModuleName :: ModuleName
emptyModuleName = ModuleName ""

unModuleName :: ModuleName -> String
unModuleName (ModuleName s) = s

modulePartSeparator :: Char
modulePartSeparator = '.'

-- | Produces the path under which the source code for the given module is
-- expected.
--
-- >>> modulePath (ModuleName "Data.List")
-- "Data/List.algst"
modulePath :: ModuleName -> FilePath
modulePath = unModuleName >>> map adjustSep >>> flip FP.addExtension "algst"
  where
    adjustSep c
      | c == modulePartSeparator = FP.pathSeparator
      | otherwise = c

-- | Transforms a 'FilePath' to the corresponding 'ModuleName'.
--
-- >>> moduleFromPath "Data/List.algst"
-- ModuleName "Data.List"
--
-- Any extensions on the last component will be dropped. No further cleaning of
-- paths or validity checking will be performed. This can lead to invalid
-- module names:
--
-- >>> moduleFromPath "../Data.ext/List.algst"
-- ModuleName "...Data.ext.List"
moduleFromPath :: FilePath -> ModuleName
moduleFromPath = FP.dropExtensions >>> map adjustSep >>> ModuleName
  where
    adjustSep c
      | FP.isPathSeparator c = modulePartSeparator
      | otherwise = c

newtype Unqualified = Unqualified {getUnqualified :: String}
  deriving stock (Show, Lift)
  deriving newtype (Eq, Ord, Hashable)

-- | Type level tag to differentiate between type level and value level names.
data Scope = Types | Values
  deriving stock (Eq, Ord, Show, Generic, Lift)

instance Hashable Scope

-- | Type level tag to differentiate between names as they were written by the
-- user and resolved names.
data Stage = Written | Resolved
  deriving stock (Eq, Ord, Generic)

instance Hashable Stage

$(genSingletons [''Scope, ''Stage])

type ScopeIndexed :: Type -> (Scope -> Type) -> Constraint
class ScopeIndexed t f | t -> f where
  typesScopeL :: L.Lens' t (f Types)
  valuesScopeL :: L.Lens' t (f Values)

instance ScopeIndexed (f Types, f Values) f where
  typesScopeL = L._1
  valuesScopeL = L._2

type ScopedVariants :: (Scope -> Type) -> Type
type ScopedVariants f = (f Types, f Values)

scopedVariants :: (forall scope. SingI scope => f scope) -> ScopedVariants f
scopedVariants x = (x, x)

scopeL :: forall scope t f. (SingI scope, ScopeIndexed t f) => L.Lens' t (f scope)
scopeL = scopeL' (sing @scope)
{-# INLINE scopeL #-}

scopeL' :: ScopeIndexed t f => Sing scope -> L.Lens' t (f scope)
scopeL' = \case
  STypes -> typesScopeL
  SValues -> valuesScopeL
{-# INLINE scopeL' #-}

newtype ResolvedId = ResolvedId Word
  deriving stock (Eq, Ord, Show, Generic, Lift)

instance Hashable ResolvedId

firstResolvedId :: ResolvedId
firstResolvedId = ResolvedId 0

nextResolvedId :: ResolvedId -> ResolvedId
nextResolvedId (ResolvedId w) = ResolvedId (w + 1)

{- ORMOLU_DISABLE -}
type NameW = Name Written
type NameR = Name Resolved
type ProgVar stage = Name stage Values
type TypeVar stage = Name stage Types
{- ORMOLU_ENABLE -}

type Name :: Stage -> Scope -> Type
data Name stage scope where
  -- | An unresolved name. Represents the module and unqualified part as they
  -- were written by the user.
  Name :: ModuleName -> Unqualified -> Name Written scope
  -- | A resolved name combines information about how a name was written
  -- by the user with definitive globally identifying information.
  --
  -- All type/value constructors and variables are uniquely identified by
  -- their origin module and a module-unique 'ResolvedId'.
  --
  -- Equalitiy, ordering and hashing does not consider the "written" component,
  -- it is purely cosmetic.
  ResolvedName :: Name Written scope -> ModuleName -> !ResolvedId -> Name Resolved scope

pattern UnqualifiedName :: Unqualified -> Name Written scope
pattern UnqualifiedName u = Name (ModuleName "") u

deriving stock instance Show (Name stage scope)
deriving stock instance Lift (Name stage scope)

instance Eq (Name stage scope) where
  a == b = compare a b == EQ

instance Ord (Name stage scope) where
  compare (Name mod un) (Name mod' un') =
    compare mod mod' <> compare un un'
  compare (ResolvedName _ mod ri) (ResolvedName _ mod' ri') =
    compare ri ri' <> compare mod mod'

instance Hashable (Name stage scope) where
  hashWithSalt s (Name mod un) =
    s
      `hashWithSalt` (1 :: Int)
      `hashWithSalt` mod
      `hashWithSalt` un
  hashWithSalt s (ResolvedName _ mod ri) =
    s
      `hashWithSalt` (2 :: Int)
      `hashWithSalt` mod
      `hashWithSalt` ri

pattern Wildcard :: Name Written scope
pattern Wildcard = Name (ModuleName "") (Unqualified "_")

pattern PairCon :: Name Written scope
pattern PairCon = Name (ModuleName "") (Unqualified "(,)")

-- | Checks wether the given name is a wildcard pattern.
--
-- FIXME: Defering to 'nameWritten' feels quite fragile.
isWild :: Name stage scope -> Bool
isWild (nameWritten -> Wildcard) = True
isWild _ = False

nameWritten :: Name stage scope -> Name Written scope
nameWritten = L.view nameWrittenL

nameUnqualified :: Name stage scope -> Unqualified
nameUnqualified = L.view nameUnqualifiedL

nameWrittenModule :: Name stage scope -> ModuleName
nameWrittenModule = L.view nameWrittenModuleL

nameResolvedModule :: Name Resolved scope -> ModuleName
nameResolvedModule = L.view nameResolvedModuleL

nameResolvedId :: Name Resolved scope -> ResolvedId
nameResolvedId = L.view nameResolvedIdL

nameWrittenL :: L.Lens' (Name stage scope) (Name Written scope)
nameWrittenL f n@Name {} = f n
nameWrittenL f (ResolvedName n m r) = (\n' -> ResolvedName n' m r) <$> f n
{-# INLINE nameWrittenL #-}

nameUnqualifiedL :: L.Lens' (Name stage scope) Unqualified
nameUnqualifiedL =
  nameWrittenL . L.lens (\(Name _ u) -> u) (\(Name m _) u -> Name m u)
{-# INLINE nameUnqualifiedL #-}

nameWrittenModuleL :: L.Lens' (Name stage scope) ModuleName
nameWrittenModuleL =
  nameWrittenL . L.lens (\(Name m _) -> m) (\(Name _ u) m -> Name m u)
{-# INLINE nameWrittenModuleL #-}

nameResolvedModuleL :: L.Lens' (Name Resolved scope) ModuleName
nameResolvedModuleL =
  L.lens
    (\(ResolvedName _ m _) -> m)
    (\(ResolvedName n _ r) m -> ResolvedName n m r)
{-# INLINE nameResolvedModuleL #-}

nameResolvedIdL :: L.Lens' (Name Resolved scope) ResolvedId
nameResolvedIdL =
  L.lens
    (\(ResolvedName _ _ r) -> r)
    (\(ResolvedName n m _) r -> ResolvedName n m r)
{-# INLINE nameResolvedIdL #-}

pprName :: Name stage scope -> String
pprName (nameWritten -> n) = fold modulePrefix ++ getUnqualified (nameUnqualified n)
  where
    modulePrefix :: Maybe String
    modulePrefix = do
      guard $ not $ isWild n
      guard $ not $ null $ unModuleName $ nameWrittenModule n
      pure $ unModuleName (nameWrittenModule n) ++ "."

pprResolved :: Name Resolved scope -> String
pprResolved (ResolvedName _ (ModuleName m) (ResolvedId r)) =
  m ++ '.' : show r

-- TODO: Check if there is difference in runtime/allocation when switching
-- between ordered and unorderered maps.

-- | A map from names in the given scope.
type NameMap scope = NameMapG Written scope

-- | A set of names in the given scope.
type NameSet scope = NameSetG Written scope

-- | Either a value or type name.
type AName = ANameG Written

-- | A map which can have type and value names as its keys.
type ANameMap = Map.Map AName

-- | A set which can contain type and value names.
type ANameSet = Set.Set AName

{- ORMOLU_DISABLE -}
type NameMapG stage scope = Map.Map (Name stage scope)
type NameSetG stage scope = Set.Set (Name stage scope)
type ANameG stage = Either (Name stage Types) (Name stage Values)
type ANameMapG stage = Map.Map (ANameG stage)
type ANameSetG stage = Set.Set (ANameG stage)
{- ORMOLU_ENABLE -}

type ANameLike :: Type -> Stage -> Constraint
class ANameLike name stage where
  foldName :: (Name stage Types -> a) -> (Name stage Values -> a) -> name -> a

instance (stage ~ stage', SingI scope) => ANameLike (Name stage scope) stage' where
  foldName f g n = eitherName @scope (f n) (g n)

instance stage ~ stage' => ANameLike (ANameG stage) stage' where
  foldName = either

eitherName :: forall s a. SingI s => (s ~ Types => a) -> (s ~ Values => a) -> a
eitherName tv pv = case sing @s of
  STypes -> tv
  SValues -> pv

liftName :: ANameLike name stage => name -> ANameG stage
liftName = foldName Left Right

liftNameSet :: SingI scope => NameSetG stage scope -> ANameSetG stage
liftNameSet = Set.mapMonotonic liftName

liftNameMap :: SingI scope => NameMapG stage scope a -> ANameMapG stage a
liftNameMap = Map.mapKeysMonotonic liftName
