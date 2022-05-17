{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module AlgST.Typing.Monad where

import AlgST.Rename
import AlgST.Rename.Fresh
import AlgST.Syntax.Decl
import AlgST.Syntax.Kind qualified as K
import AlgST.Syntax.Name
import AlgST.Syntax.Program
import AlgST.Typing.Phase
import AlgST.Util.ErrorMessage (Diagnostic)
import AlgST.Util.Lenses
import Control.Monad.Eta
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Validate
import Data.DList.DNonEmpty (DNonEmpty)
import Data.Map qualified as Map
import Data.Sequence (Seq)
import Data.These
import Lens.Family2
import Syntax.Base

-- | A @Var@ tracks a 'ProgVar's type, declaration location and usage
-- information.
data Var = Var
  { varType :: TcType,
    varUsage :: !Usage,
    varLocation :: Pos
  }

data Usage
  = -- | Usage for 'Un' variables is not tracked.
    UnUsage
  | -- | An unused 'Lin' variable.
    LinUnunsed
  | -- | A used 'Lin' variable, associated with the usage location.
    LinUsed Pos

type TypeEnv = TcNameMap Values Var

type KindEnv = TcNameMap Types K.Kind

-- | Enivronment for all typing operations.
--
-- The two context fields 'tcContext' and 'tcCheckedContext' hold the same
-- information. The need to carry both arises because when checking the context
-- itself we need access to the declarations. More concretly this allows the
-- checked declarations to refer to each other mutually recursively viw
-- 'TypeRef' nodes. This is achieved via lazyness which allows to access the
-- checked program during its checking. At the same time all strict lookups
-- have to go through the unchecked, readily available 'tcContext'.
data KiTypingEnv = KiTypingEnv
  { -- | Maps type variables to their kind.
    tcKindEnv :: KindEnv,
    tcContext :: RnModule,
    -- | The stack of type aliases we are expanding. The first two tuple
    -- elements are declaration location and name.
    tcExpansionStack :: Seq (Pos, TypeVar TcStage, TypeAlias Rn)
  }

data TcValue
  = ValueGlobal (Maybe (ValueDecl Rn)) TcType
  | ValueCon (ConstructorDecl Tc)

-- | Like 'ValuesMap' but the values in the map are of type 'TcValue'.
type TcValuesMap = TcNameMap Values TcValue

data TyTypingEnv = TyTypingEnv
  { tcKiTypingEnv :: KiTypingEnv,
    -- | All known types, fully checked.
    tcCheckedTypes :: TypesMap Tc,
    -- | Associates globals with their type.
    --
    -- 'Left' values represent protocol constructors. These don't form valid
    -- expressions. The associated value is the parent type's name.
    tcCheckedValues :: TcNameMap Values TcValue
  }

newtype KiSt = KiSt {tcAliases :: TcNameMap Types Alias}

-- | State during type checking of expressions. The main part is the 'TypeEnv'
-- which maps the variables in scope to their 'Usage' status.
--
-- When new fields are introduced 'parallelTypeM' might have to be adjusted.
data TySt = TySt
  { tcTypeEnv :: TypeEnv,
    tcKindSt :: KiSt
  }

data Alias
  = CheckedAlias !(TypeAlias Tc) !K.Kind
  | -- TODO: Should this carry an Origin instead? I think we can assume that
    -- an imported/builtin alias won't fail to expand. In that case a simple
    -- Pos is enough information for the error messages.
    UncheckedAlias !Pos !(TypeAlias Rn)
  | ExpandingAlias !Int
  | RecursiveAlias RecursiveSets

data RecursiveSets
  = RecursiveSets
      (TcNameSet Types)
      [(Pos, TypeVar TcStage, TypeAlias Rn)]
      !(Map.Map (TcNameSet Types) [(Pos, TypeVar TcStage, TypeAlias Rn)])

instance Semigroup RecursiveSets where
  RecursiveSets a b recs <> RecursiveSets _ _ recs' =
    RecursiveSets a b (recs <> recs')

{- ORMOLU_DISABLE -}
makeLenses ['tcKindEnv, 'tcExpansionStack] ''KiTypingEnv
tcKindEnvL :: Lens' KiTypingEnv KindEnv
tcExpansionStackL :: Lens' KiTypingEnv (Seq (Pos, TypeVar TcStage, TypeAlias Rn))

makeLenses ['tcKiTypingEnv] ''TyTypingEnv
tcKiTypingEnvL :: Lens' TyTypingEnv KiTypingEnv

makeLenses ''KiSt
tcAliasesL :: Lens' KiSt (TcNameMap Types Alias)

makeLenses ''TySt
tcTypeEnvL :: Lens' TySt TypeEnv
tcKindStL :: Lens' TySt KiSt
{- ORMOLU_ENABLE -}

class HasKiSt st where
  kiStL :: Lens' st KiSt

instance HasKiSt KiSt where
  kiStL = id
  {-# INLINE kiStL #-}

instance HasKiSt TySt where
  kiStL = tcKindStL
  {-# INLINE kiStL #-}

class HasKiEnv env where
  kiEnvL :: Lens' env KiTypingEnv

instance HasKiEnv KiTypingEnv where
  kiEnvL = id
  {-# INLINE kiEnvL #-}

instance HasKiEnv TyTypingEnv where
  kiEnvL = tcKiTypingEnvL

type TypeM = TcM TyTypingEnv TySt

type TcM env s = ValidateT Errors (StateT s (ReaderT env Fresh))

type Errors = These (DNonEmpty Diagnostic) RecursiveSets

liftFresh :: Fresh a -> TcM env st a
liftFresh = etaTcM . lift . lift . lift
{-# INLINE liftFresh #-}

etaTcM :: TcM env s a -> TcM env s a
etaTcM = etaValidateT . mapValidateT (etaStateT . mapStateT (etaReaderT . mapReaderT etaFresh))
{-# INLINE etaTcM #-}
