{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module AlgST.Util.SourceLocation
  ( -- * Locations
    SrcLoc (NullLoc, ..),
    advanceLoc,

    -- * Ranges
    SrcRange (SizedRange, NullRange, ..),
    runion,
    rangeByteCount,
    unsafeRangeString,

    -- * Extracting ranges
    HasRange (..),
    StoresRange (..),
    genericRangeL,
    Generically (..),
    GHasRange,
    GStoresRange,

    -- * @ByteString@ interactions
    startLoc,
    fullRange,
    unsafeBasePtr,

    -- * @Located@
    Located (..),
    unL,
    onUnL,
    foldL,
    uncurryL,
    (@-),

    -- * Pos transition
    needRange,
    needPos,
    needLoc,
    needPLoc,
  )
where

import AlgST.Syntax.Pos qualified as P
import AlgST.Util
import AlgST.Util.Generically
import Control.Foldl.NonEmpty qualified as L1
import Data.ByteString qualified as BS
import Data.ByteString.Internal (ByteString (..))
import Data.CallStack (HasCallStack)
import Data.Coerce
import Data.Hashable
import Data.Profunctor
import Data.Semigroup
import Data.Void
import Data.Word
import Foreign
import GHC.Foreign qualified as GHC
import GHC.ForeignPtr
import GHC.Generics
import GHC.TypeLits
import Language.Haskell.TH.Syntax (Lift (..))
import Lens.Family2 hiding (to)
import Lens.Family2.Unchecked (lens)
import System.IO qualified as IO
import System.IO.Unsafe

newtype SrcLoc = SrcLoc {locPtr :: Ptr Word8}
  deriving newtype (Eq, Ord, Hashable)

pattern NullLoc :: SrcLoc
pattern NullLoc <- SrcLoc ((== nullPtr) -> True)
  where
    NullLoc = SrcLoc nullPtr

-- | Every @SrcLoc@ is lifted as 'NullLoc'.
instance Lift SrcLoc where
  liftTyped _ = [||NullLoc||]

-- | This instance is only meant for debugging purposes. The output
-- unconditionally contains colorizing escape sequences: to easier distinguish
-- the different pointer values they are colorized semi-randomly based on their
-- value.
instance Show SrcLoc where
  showsPrec p (SrcLoc ptr) = showParen (p > 10) do
    showString "SrcLoc " . coloredPtr ptr

coloredPtr :: Ptr Word8 -> ShowS
coloredPtr p =
  showString "\ESC[38;5;"
    . shows color
    . showChar 'm'
    . shows p
    . showString "\ESC[m"
  where
    WordPtr color = ptrToWordPtr p * (ptrToWordPtr p + 3) `mod` (228 - 21) + 21

advanceLoc :: SrcLoc -> Int -> SrcLoc
advanceLoc = coerce plusPtr

data SrcRange = SrcRange !SrcLoc !SrcLoc
  deriving stock (Eq, Ord, Generic, Lift)

-- | A range of size zero, located at 'NullLoc'.
pattern NullRange :: SrcRange
pattern NullRange = SrcRange NullLoc NullLoc

-- | Constructs or deconstructs a 'SrcRange' from a start location and a size.
pattern SizedRange :: SrcLoc -> Int -> SrcRange
pattern SizedRange start size <-
  ((,) <$> getStartLoc <*> rangeByteCount -> (!start, !size))
  where
    SizedRange start size = SrcRange start (start `advanceLoc` size)

{-# COMPLETE SizedRange #-}

instance Semigroup SrcRange where
  SrcRange s1 e1 <> SrcRange s2 e2 =
    SrcRange (min s1 s2) (max e1 e2)

  sconcat = L1.fold1 do
    SrcRange
      <$> lmap getStartLoc L1.minimum
      <*> lmap getEndLoc L1.maximum

  stimes = stimesIdempotent

runion :: (HasRange a, HasRange b) => a -> b -> SrcRange
runion a b = getRange a <> getRange b

-- | This instance is only meant for debugging purposes. The output
-- unconditionally contains colorizing escape sequences (see the @Show
-- 'SrcLoc'@ instance documentation for more information).
instance Show SrcRange where
  showsPrec p (SizedRange start sz) = showParen (p > 10) do
    showString "SrcRange "
      . coloredPtr (locPtr start)
      . showString " ("
      . shows sz
      . showChar ' '
      . plural sz (showString "byte") (showString "bytes")
      . showChar ')'

instance Hashable SrcRange

-- | Counts the number of bytes included in the range.
rangeByteCount :: SrcRange -> Int
rangeByteCount r = locPtr (getEndLoc r) `minusPtr` locPtr (getStartLoc r)

startLoc :: ByteString -> SrcLoc
startLoc = SrcLoc . unsafeBasePtr

fullRange :: ByteString -> SrcRange
fullRange bs = SrcRange (startLoc bs) (startLoc bs `advanceLoc` BS.length bs)

unsafeRangeString :: SrcRange -> String
unsafeRangeString r = unsafeDupablePerformIO do
  GHC.peekCStringLen IO.utf8 (castPtr (locPtr (getStartLoc r)), rangeByteCount r)

unsafeBasePtr :: ByteString -> Ptr Word8
unsafeBasePtr (PS fp offset _) = unsafeForeignPtrToPtr fp `plusPtr` offset

class HasRange a where
  {-# MINIMAL getRange | getStartLoc, getEndLoc #-}

  getRange :: a -> SrcRange
  getRange a = SrcRange (getStartLoc a) (getEndLoc a)

  getStartLoc :: a -> SrcLoc
  getStartLoc (getRange -> SrcRange x _) = x

  getEndLoc :: a -> SrcLoc
  getEndLoc (getRange -> SrcRange _ x) = x

class HasRange a => StoresRange a where
  rangeL :: Lens' a SrcRange

instance HasRange SrcRange where
  getRange = id

instance StoresRange SrcRange where
  rangeL = id

instance HasRange Void where
  getRange = absurd

instance StoresRange Void where
  rangeL = lens absurd const

instance (HasRange a, HasRange b) => HasRange (Either a b) where
  getRange = either getRange getRange
  getStartLoc = either getStartLoc getStartLoc
  getEndLoc = either getEndLoc getEndLoc

instance (Generic a, GHasRange (Rep a)) => HasRange (Generically a) where
  getRange (Generically a) = getRangeRep (from a)

genericRangeL :: (Generic a, GStoresRange (Rep a), HasRange a) => Lens' a SrcRange
genericRangeL = lens getRange \a -> to . updateRangeRep (from a)

type GenericallyStoresRangeError a =
  ( Text "Deriving ‘"
      :<>: ShowType (StoresRange a)
      :<>: Text "’ via ‘"
      :<>: ShowType (Generically a)
      :<>: Text "’ is not possible"
  )
    :$$: Text "Use ‘genericRangeL’ instead as an implementation for ‘rangeL’"

instance (TypeError (GenericallyStoresRangeError a), Generic a, GStoresRange (Rep a)) => StoresRange (Generically a) where
  rangeL = error "not possible"

-- | Extracts the @SrcRange@ from the leftmost component of each constructor.
class GHasRange f where
  getRangeRep :: f a -> SrcRange

instance (GHasRange f, GHasRange g) => GHasRange (f :+: g) where
  getRangeRep (L1 f) = getRangeRep f
  getRangeRep (R1 g) = getRangeRep g

instance GHasRange f => GHasRange (f :*: g) where
  getRangeRep (f :*: _) = getRangeRep f

instance GHasRange f => GHasRange (M1 i c f) where
  getRangeRep (M1 f) = getRangeRep f

instance HasRange c => GHasRange (K1 i c) where
  getRangeRep (K1 c) = getRange c

class GHasRange f => GStoresRange f where
  updateRangeRep :: f a -> SrcRange -> f a

instance (GStoresRange f, GStoresRange g) => GStoresRange (f :+: g) where
  updateRangeRep (L1 f) = L1 . updateRangeRep f
  updateRangeRep (R1 g) = R1 . updateRangeRep g

instance GStoresRange f => GStoresRange (f :*: g) where
  updateRangeRep (f :*: g) = (:*: g) . updateRangeRep f

instance GStoresRange f => GStoresRange (M1 i c f) where
  updateRangeRep (M1 f) = M1 . updateRangeRep f

instance StoresRange c => GStoresRange (K1 i c) where
  updateRangeRep (K1 c) r = K1 $ c & rangeL .~ r

-- | Attaches a position to a value of type @a@.
--
-- Ordering/Equality is not defined for this type to avoid confusion wether the
-- position is considered or not. The function 'onUnL' is provided to
-- simplify comparing and similar functions which do not consider the
-- position.
data Located a = !SrcRange :@ a
  deriving stock (Show, Lift)
  deriving stock (Functor, Foldable, Traversable)

instance HasRange (Located a) where
  getRange (r :@ _) = r

instance StoresRange (Located a) where
  rangeL = lens getRange \(_ :@ a) r -> r :@ a

infix 9 :@, @-

unL :: Located a -> a
unL (_ :@ a) = a

foldL :: (a -> b) -> Located a -> b
foldL f = f . unL

uncurryL :: (SrcRange -> a -> b) -> Located a -> b
uncurryL f (p :@ a) = f p a

onUnL :: (a -> a -> b) -> Located a -> Located a -> b
onUnL f (_ :@ x) (_ :@ y) = f x y

(@-) :: HasRange p => p -> a -> Located a
p @- a = getRange p :@ a

needPos :: HasCallStack => a -> P.Pos
needPos = undefined
{-# WARNING needPos "compat placeholder" #-}

needRange :: HasCallStack => a -> SrcRange
needRange = undefined
{-# DEPRECATED needRange "compat placeholder" #-}

needLoc :: HasCallStack => P.Located a -> Located a
needLoc = undefined
{-# DEPRECATED needLoc "compat placeholder" #-}

needPLoc :: HasCallStack => Located a -> P.Located a
needPLoc = undefined
{-# DEPRECATED needPLoc "compat placeholder" #-}
