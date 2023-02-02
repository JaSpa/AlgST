{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module AlgST.Syntax.Module
  ( -- * Modules
    Module (..),
    emptyModule,
    TypesMap,
    ValuesMap,
    SignaturesMap,
    ForallX,
    Benchmark (..),

    -- * Imports
    Import (..),
    ImportSelection (..),
    ImportKey,
    ImportHidden,
    ImportRenamed,
    ImportBehaviour (..),
  )
where

import AlgST.Syntax.Decl qualified as D
import AlgST.Syntax.Expression qualified as E
import AlgST.Syntax.Name
import AlgST.Syntax.Phases
import AlgST.Syntax.Type qualified as T
import AlgST.Util.SourceLocation
import Data.HashMap.Strict (HashMap)
import Data.Map.Strict qualified as Map
import Data.Semigroup
import Instances.TH.Lift ()
import Language.Haskell.TH.Syntax (Lift)

-- | Groups the @ForallX@ constraint synonym from "AlgST.Syntax.Decl",
-- "AlgST.Syntax.Type", and "AlgST.Syntax.Expression".
type ForallX :: CAll
type ForallX c x =
  ( D.ForallDeclX c x,
    D.ForallConX c x,
    T.ForallX c x,
    E.ForallX c x
  )

-- | Describes a single import statement.
--
-- @Import@ is parameterized over the imported thing which could be either an
-- unresolved module name or a resolved module.
data Import a = Import
  { -- | The imported thing.
    importTarget :: a,
    -- | The qualifier for imported identifiers. Can be empty (@'ModuleName' ""@)
    -- in which case names are imported unqualified.
    importQualifier :: ModuleName,
    -- | The set of imported identifiers.
    importSelection :: ImportSelection
  }
  deriving stock (Functor, Foldable, Traversable, Show, Lift)

-- | Describes the set of imported identifiers.
data ImportSelection
  = -- | Import all public definitions from this module and rename/hide
    -- identifiers as specified in the 'ImportItem's.
    --
    -- The 'Pos' gives the location of the @*@ token indicating the
    -- @ImportAll@. In the case of no import list it points to the beginning of
    -- the import statement.
    --
    -- The set contains the hidden identifiers. The hidden identifiers include
    -- all renamed identifiers. The map contains renamed identifiers. The map's
    -- keys are the new names.
    ImportAll !SrcRange !ImportHidden !ImportRenamed
  | -- | Import only the names specified, potentially renaming imported
    -- identifiers.
    --
    -- The map contains the identifiers to be imported. If no renaming is
    -- applied the two 'Unqualified' components are the same. Otherwise the
    -- value points to the original name and the key gives the new name.
    ImportOnly !ImportRenamed
  deriving stock (Show, Lift)

type ImportKey = (Scope, Unqualified)

type ImportHidden = HashMap ImportKey SrcRange

type ImportRenamed = HashMap ImportKey (Located Unqualified)

-- | Describes the import behaviour regarding a single identifier.
data ImportBehaviour
  = -- | Import the associated identifier.
    ImportAsIs
  | -- | @ImportFrom target@ imports identifier @target@ under the new name.
    ImportFrom Unqualified
  | -- | Hide the associated identifier in case the whole module is imported.
    ImportHide
  deriving stock (Show, Lift)

data Module x = Module
  { moduleTypes :: !(TypesMap x),
    moduleValues :: !(ValuesMap x),
    moduleSigs :: !(SignaturesMap x),
    moduleBench :: [Benchmark x]
  }

deriving stock instance (ForallX Lift x) => Lift (Module x)

-- | Multiple modules can be merged. This is only safe, however, if the names
-- the maps are keyed by are resolved.
instance XStage x ~ Resolved => Semigroup (Module x) where
  Module a1 b1 c1 d1 <> Module a2 b2 c2 d2 =
    Module (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2)

  stimes = stimesIdempotentMonoid

instance XStage x ~ Resolved => Monoid (Module x) where
  mempty = emptyModule

emptyModule :: Module x
emptyModule = Module Map.empty Map.empty Map.empty []

data Benchmark x = Benchmark
  { benchName :: String,
    -- | A @benchExpect = False@ indicates that we should check that the two
    -- types are not equivalent before benchmarking.
    benchExpect :: Bool,
    benchT1 :: T.Type x,
    benchT2 :: T.Type x
  }

deriving stock instance (T.ForallX Lift x) => Lift (Benchmark x)

-- | Mapping between type names and the type declarations.
type TypesMap x = NameMapG (XStage x) Types (D.TypeDecl x)

-- | Mapping between value names and their declaration, which is either a
-- constructor or a value/function binding.
type ValuesMap x = NameMapG (XStage x) Values (Either (D.ConstructorDecl x) (D.ValueDecl x))

-- | Mapping between value/function names and their signatures.
type SignaturesMap x = NameMapG (XStage x) Values (D.SignatureDecl x)
