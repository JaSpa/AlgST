{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module AlgST.Typing.Monad where

import AlgST.Rename
import AlgST.Rename.Fresh
import AlgST.Syntax.Decl
import AlgST.Syntax.Kind qualified as K
import AlgST.Syntax.Module
import AlgST.Syntax.Name
import AlgST.Typing.Phase
import AlgST.Util.Diagnose qualified as D
import AlgST.Util.Lenses
import AlgST.Util.SourceLocation
import Control.Monad.Eta
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Validate
import Data.Map qualified as Map
import Data.Sequence (Seq)
import Data.These
import Language.Haskell.TH.Syntax (Lift)
import Lens.Family2

-- | A @Var@ tracks a 'ProgVar's type, declaration location and usage
-- information.
data Var = Var
  { varType :: !TcType,
    varUsage :: !Usage,
    varLocation :: !SrcRange
  }

instance HasRange Var where
  getRange = varLocation

data Usage
  = -- | Usage for 'Un' variables is not tracked.
    UnUsage
  | -- | An unused 'Lin' variable.
    LinUnunsed
  | -- | A used 'Lin' variable, associated with the usage location.
    LinUsed SrcRange

-- | Describes the various kinds of (pseudo) type constructors.
--
-- During type checking, all proper type constructors are mapped to
-- 'NominalTypeCon' and all type aliases are, initially, mapped to 'LazyAlias'.
-- When type alias resultion happens the mapping will be updated first to
-- 'ExpandingAlias' and then to either 'ResolvedAlias' or 'CyclicAlias'.
data TypeCon
  = -- | A constructor for a nominal type. There is a sequence of parameters,
    -- a resulting 'K.Kind', and a set of constructor names.
    NominalTypeCon !(Params Resolved) !K.Kind !(TcNameSet Values)
  | -- | A resolved type alias annotated with the actual infered 'K.Kind'.
    ResolvedAlias !(TypeAlias Tc) !K.Kind
  | -- | An unresolved type alias annotated with the range of the definition's
    -- LHS and the definitions RHS.
    LazyAlias !SrcRange !(TypeAlias Rn)
  | -- | An alias that is being expanded. The 'Int' is the index in the
    -- 'tcExpansionStack' were the expansion began.
    ExpandingAlias !Int
  | -- | A type alias that was discovered to be cyclic.
    CyclicAlias RecursiveSets
  deriving stock (Lift)

type TypeEnv = TcNameMap Values Var

type KindEnv = TcNameMap Types K.Kind

-- | Enivronment for all typing operations.
data KiTypingEnv = KiTypingEnv
  { -- | Maps type variables to their kind.
    tcKindEnv :: KindEnv,
    -- | The stack of type aliases we are expanding. We keep track of this so
    -- that we can diagnose the actual cycles in type synonyms.
    tcExpansionStack :: Seq ExpansionEntry
  }

emptyKiTypingEnv :: KiTypingEnv
emptyKiTypingEnv =
  KiTypingEnv
    { tcKindEnv = mempty,
      tcExpansionStack = mempty
    }

data TcValue
  = ValueGlobal (Maybe (ValueDecl Rn)) TcType
  | ValueCon (ConstructorDecl Tc)
  deriving stock (Lift)

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

newtype KiSt = KiSt
  { tcTypeCons :: TcNameMap Types TypeCon
  }

data TySt = TySt
  { tcKindSt :: !KiSt,
    -- | Maps variables in scope to their 'Usage' status.
    tcTypeEnv :: !TypeEnv
  }

-- | @RecursiveSets@ is a mapping from @'TcNameSet' 'Types'@ to
-- @['ExpansionEntry']@.
--
-- The mapping is non-empty. This is achieved by replicating one pair in the
-- mapping in the first two constructor elements.
data RecursiveSets
  = RecursiveSets
      (TcNameSet Types)
      [ExpansionEntry]
      !(Map.Map (TcNameSet Types) [ExpansionEntry])
  deriving stock (Lift)

instance Semigroup RecursiveSets where
  RecursiveSets a b recs <> RecursiveSets _ _ recs' =
    RecursiveSets a b (recs <> recs')

type TypeM = TcM TyTypingEnv TySt

type TcM env st = ValidateT Errors (StateT st (ReaderT env Fresh))

type Errors = These D.DErrors RecursiveSets

-- | An entry in the current stack of type alias expansions.
--
-- The very bottom element of the stack will not have a meaningfull
-- 'expansionUseRange'.
data ExpansionEntry = ExpansionEntry
  { expansionUseRange :: !SrcRange,
    expansionDefRange :: !SrcRange,
    expansionName :: !(NameR Types),
    expansionAlias :: !(TypeAlias Rn)
  }
  deriving stock (Lift)

{- ORMOLU_DISABLE -}
makeLenses ''KiTypingEnv
tcKindEnvL :: Lens' KiTypingEnv KindEnv
tcExpansionStackL :: Lens' KiTypingEnv (Seq ExpansionEntry)

makeLenses ''KiSt
tcTypeConsL :: Lens' KiSt (TcNameMap Types TypeCon)

makeLenses ['tcKiTypingEnv] ''TyTypingEnv
tcKiTypingEnvL :: Lens' TyTypingEnv KiTypingEnv 

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

liftFresh :: Fresh a -> TcM env st a
liftFresh = etaTcM . lift . lift . lift
{-# INLINE liftFresh #-}

etaTcM :: TcM env st a -> TcM env st a
etaTcM = etaValidateT . mapValidateT (etaStateT . mapStateT (etaReaderT . mapReaderT etaFreshT))
{-# INLINE etaTcM #-}
