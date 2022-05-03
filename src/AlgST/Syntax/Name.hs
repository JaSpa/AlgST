{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
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
    Name (..),
    pprName,
    pattern Wildcard,
    isWild,
    pattern PairCon,

    -- ** Unqualified parts
    Unqualified (..),

    -- ** Resolved Names
    ResolvedName (..),
    pprNameResolved,
    ResolvedId,
    firstResolvedId,
    nextResolvedId,

    -- ** Abbreviations
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

    -- * Modules
    Module (..),
    moduleName,
    modulePath,
    moduleFromPath,

    -- * Type Level Tags
    Scope (..),
    SScope (..),
    TypesSym0,
    ValuesSym0,
  )
where

import Control.Category ((>>>))
import Control.Monad
import Data.Foldable
import Data.Hashable
import Data.Kind
import Data.Map.Strict qualified as Map
import Data.Ord
import Data.Set qualified as Set
import Data.Singletons.TH
import GHC.Generics (Generic)
import Language.Haskell.TH.Syntax (Lift)
import System.FilePath qualified as FP

-- | The name of a module.
newtype Module = Module String
  deriving stock (Show, Lift)
  deriving newtype (Eq, Ord, Hashable)

moduleName :: Module -> String
moduleName (Module s) = s

modulePartSeparator :: Char
modulePartSeparator = '.'

-- | Produces the path under which the source code for the given module is
-- expected.
--
-- >>> modulePath (Module "Data.List")
-- "Data/List.algst"
modulePath :: Module -> FilePath
modulePath = moduleName >>> map adjustSep >>> flip FP.addExtension "algst"
  where
    adjustSep c
      | c == modulePartSeparator = FP.pathSeparator
      | otherwise = c

-- | Transforms a 'FilePath' to the corresponding 'Module'.
--
-- >>> moduleFromPath "Data/List.algst"
-- Module "Data.List"
--
-- Any extensions on the last component will be dropped. No further cleaning of
-- paths or validity checking will be performed. This can lead to invalid
-- module names:
--
-- >>> moduleFromPath "../Data.ext/List.algst"
-- Module "...Data.ext.List"
moduleFromPath :: FilePath -> Module
moduleFromPath = FP.dropExtensions >>> map adjustSep >>> Module
  where
    adjustSep c
      | FP.isPathSeparator c = modulePartSeparator
      | otherwise = c

newtype Unqualified = Unqualified {getUnqualified :: String}
  deriving stock (Show, Lift)
  deriving newtype (Eq, Ord, Hashable)

-- | Type level tag to differentiate between type level and value level names.
data Scope = Types | Values
  deriving (Eq, Ord, Generic)

instance Hashable Scope

$(genSingletons [''Scope])

-- | A resolved name combines information about how a name was written
-- by the user with definitive globally identifying information.
--
-- All type/value constructors and variables are uniquely identified by
-- their origin module and a module-unique 'ResolvedId'.
type ResolvedName :: Scope -> Type
data ResolvedName scope = ResolvedName
  { resolvedModule :: Module,
    resolvedId :: !ResolvedId,
    -- | The name as it was written by the user. This name is used when
    -- shown in diagnostics.
    --
    -- This field is not considered for equality, ordering or when
    -- calculating hash values.
    nameUnresolved :: Name scope
  }
  deriving stock (Show)

instance Eq (ResolvedName scope) where
  a == b = a `compare` b == EQ

instance Ord (ResolvedName scope) where
  compare = comparing resolvedId <> comparing resolvedModule

instance Hashable (ResolvedName scope) where
  hashWithSalt s rn =
    s `hashWithSalt` resolvedId rn
      `hashWithSalt` resolvedModule rn

newtype ResolvedId = ResolvedId Word
  deriving stock (Eq, Ord, Show, Generic)

instance Hashable ResolvedId

firstResolvedId :: ResolvedId
firstResolvedId = ResolvedId 0

nextResolvedId :: ResolvedId -> ResolvedId
nextResolvedId (ResolvedId w) = ResolvedId (w + 1)

type ProgVar = Name Values

type TypeVar = Name Types

-- | TODO: Describe why every name has a module
--
-- > data T a = T a
--
-- > foo : forall (a:TU). ...
-- > foo [a] =
-- >  let x = T [a -> a] in
-- >  ...
--
-- @x@ has type @...@ but it should look like @...@
type Name :: Scope -> Type
data Name scope = Name
  { nameModule :: Module,
    nameUnqualified :: Unqualified
  }
  deriving stock (Eq, Ord, Show, Generic, Lift)

instance Hashable (Name scope)

pattern Wildcard :: Name scope
pattern Wildcard =
  Name
    { nameModule = Module "",
      nameUnqualified = Unqualified "_"
    }

pattern PairCon :: Name scope
pattern PairCon =
  Name
    { nameModule = Module "",
      nameUnqualified = Unqualified "(,)"
    }

-- | Checks wether the given name is a wildcard pattern.
isWild :: Name scope -> Bool
isWild Wildcard = True
isWild _ = False

pprName :: Name scope -> String
pprName n = fold modulePrefix ++ getUnqualified (nameUnqualified n)
  where
    modulePrefix :: Maybe String
    modulePrefix = do
      guard $ not $ isWild n
      guard $ not $ null $ moduleName $ nameModule n
      pure $ moduleName (nameModule n) ++ "."

pprNameResolved :: ResolvedName scope -> String
pprNameResolved = pprName . nameUnresolved

-- TODO: Check if there is difference in runtime/allocation when switching
-- between ordered and unorderered maps.

type NameMap s = Map.Map (Name s)

type NameSet s = Set.Set (Name s)

type AName = Either TypeVar ProgVar

-- | A map which can have type and value names as its keys.
type ANameMap = Map.Map AName

-- | A set which can contain type and value names.
type ANameSet = Set.Set AName

class ANameLike name where
  foldName :: (TypeVar -> a) -> (ProgVar -> a) -> name -> a

instance SingI s => ANameLike (Name s) where
  foldName f g n = eitherName @s (f n) (g n)

instance ANameLike AName where
  foldName = either

eitherName :: forall s a. SingI s => (s ~ Types => a) -> (s ~ Values => a) -> a
eitherName tv pv = case sing @s of
  STypes -> tv
  SValues -> pv

liftName :: ANameLike name => name -> AName
liftName = foldName Left Right

liftNameSet :: SingI s => NameSet s -> ANameSet
liftNameSet = Set.mapMonotonic liftName

liftNameMap :: SingI s => NameMap s v -> ANameMap v
liftNameMap = Map.mapKeysMonotonic liftName
