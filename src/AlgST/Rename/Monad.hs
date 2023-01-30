{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module AlgST.Rename.Monad
  ( module AlgST.Rename.Monad,
    module AlgST.Rename.Fresh,
  )
where

import AlgST.Rename.Error qualified as Error
import AlgST.Rename.Fresh
import AlgST.Rename.Modules
import AlgST.Syntax.Name
import AlgST.Util.Diagnose (DErrors)
import AlgST.Util.Lenses
import AlgST.Util.SourceLocation
import Control.Monad.Reader
import Control.Monad.Validate
import Data.Generics.Lens.Lite
import Data.List (minimumBy)
import Data.Map.Strict qualified as Map
import Data.Ord
import Data.Semigroup
import GHC.Generics (Generic)
import Language.Haskell.TH.Syntax (Lift)
import Lens.Family2

-- | A partial resolve keeps track of a set of identifiers imported under the
-- same name.
--
-- Each identifier is annotated with where it was imported from. This gives the
-- user a bit of leeway when multiple unqualified/equal qualfied imports export
-- the same identifier. An error message is delayed to the usage site.
--
-- The usage of a map strucuture here instead of a simple list is deliberate:
-- This allows the same identifier (same origin) to be imported multiple times
-- and be used without problems. Consider:
--
-- > import M1 (*)
-- > import M1 (someName)
--
-- Although this use case is relatively contrived it would be quite stranger to
-- emit an error at the usage site of @someName@ along the lines of “Did you
-- mean @M1.someName@ or @M1.someName@?”.
--
-- This type has intentionally no 'Monoid' instance since it should always
-- contain at least one element. Its 'Semigroup' instance merges the maps and
-- chooses the first 'Error.AmbigousOrigin' in case of a duplicate import.
data PartialResolve scope
  = PartialResolve !(Map.Map (NameR scope) Error.AmbiguousOrigin)
  | UniqueLocal !(NameR scope)
  deriving stock (Show, Lift)

instance Semigroup (PartialResolve scope) where
  PartialResolve x <> PartialResolve y = PartialResolve do
    Map.unionWith (\a1 a2 -> minimumBy (comparing getRange) [a1, a2]) x y
  UniqueLocal x <> _ = UniqueLocal x
  _ <> UniqueLocal x = UniqueLocal x
  stimes = stimesIdempotent

resolvedUnique :: NameR scope -> Error.AmbiguousOrigin -> PartialResolve scope
resolvedUnique name origin = PartialResolve (Map.singleton name origin)

viewUniqueResolve :: PartialResolve scope -> Either [Error.AmbiguousOrigin] (NameR scope)
viewUniqueResolve = \case
  PartialResolve m
    | [name] <- Map.keys m -> Right name
    | otherwise -> Left (Map.elems m)
  UniqueLocal name -> Right name

-- | A map from user written names to the globally unique renamed version.
newtype Bindings scope = Bindings (NameMapG Written scope (PartialResolve scope))
  deriving newtype (Monoid)
  deriving stock (Show, Lift)

instance Semigroup (Bindings scope) where
  Bindings x <> Bindings y = Bindings $ Map.unionWith (<>) x y
  stimes = stimesIdempotentMonoid

-- | Gives access to the underlying map of a @Bindings' scope@ value.
--
-- The type is intentionally more restricted. We usually don't want to replace
-- the whole map with a differently scoped version. This should also improve
-- type inference.
_Bindings :: Lens' (Bindings scope) (NameMapG Written scope (PartialResolve scope))
_Bindings = coerced
{-# INLINE _Bindings #-}

data RenameEnv = RenameEnv
  { rnTyVars :: !(Bindings Types),
    rnProgVars :: !(Bindings Values)
  }
  deriving stock (Show, Generic, Lift)

instance Monoid RenameEnv where
  mempty = RenameEnv mempty mempty

instance Semigroup RenameEnv where
  e1 <> e2 =
    RenameEnv
      { rnTyVars = rnTyVars e1 <> rnTyVars e2,
        rnProgVars = rnProgVars e1 <> rnProgVars e2
      }

instance ScopeIndexed RenameEnv Bindings where
  typesScopeL = field @"rnTyVars"
  valuesScopeL = field @"rnProgVars"

type RnM = ValidateT DErrors (ReaderT (ModuleMap, RenameEnv) Fresh)
