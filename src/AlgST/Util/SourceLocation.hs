{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module AlgST.Util.SourceLocation
  ( -- * Locations
    SrcLoc (NullLoc, ..),
    advanceLoc,

    -- * Ranges
    SrcRange (SizedRange, NullRange, ..),
    rangeByteCount,
    unsafeRangeString,

    -- * Extracting ranges
    HasRange (..),
    Generically (..),
    GHasRange,

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
  )
where

import AlgST.Util
import AlgST.Util.Generically
import Data.ByteString qualified as BS
import Data.ByteString.Internal (ByteString (..))
import Data.Coerce
import Data.Hashable
import Data.Void
import Data.Word
import Foreign
import GHC.Foreign qualified as GHC
import GHC.ForeignPtr
import GHC.Generics
import Language.Haskell.TH.Syntax (Lift (..))
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

data SrcRange = SrcRange {rangeStart, rangeEnd :: !SrcLoc}
  deriving stock (Eq, Ord, Generic, Lift)

-- | A range of size zero, located at 'NullLoc'.
pattern NullRange :: SrcRange
pattern NullRange = SrcRange NullLoc NullLoc

-- | Constructs or deconstructs a 'SrcRange' from a start location and a size.
pattern SizedRange :: SrcLoc -> Int -> SrcRange
pattern SizedRange start size <-
  ((,) <$> rangeStart <*> rangeByteCount -> (!start, !size))
  where
    SizedRange start size = SrcRange start (start `advanceLoc` size)

{-# COMPLETE SizedRange #-}

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
rangeByteCount r = locPtr (rangeEnd r) `minusPtr` locPtr (rangeStart r)

startLoc :: ByteString -> SrcLoc
startLoc = SrcLoc . unsafeBasePtr

fullRange :: ByteString -> SrcRange
fullRange bs = SrcRange (startLoc bs) (startLoc bs `advanceLoc` BS.length bs)

unsafeRangeString :: SrcRange -> String
unsafeRangeString r = unsafeDupablePerformIO do
  GHC.peekCStringLen IO.utf8 (castPtr (locPtr (rangeStart r)), rangeByteCount r)

unsafeBasePtr :: ByteString -> Ptr Word8
unsafeBasePtr (PS fp offset _) = unsafeForeignPtrToPtr fp `plusPtr` offset

class HasRange a where
  {-# MINIMAL getRange | getStartLoc, getEndLoc #-}

  getRange :: a -> SrcRange
  getRange a = SrcRange (getStartLoc a) (getEndLoc a)

  getStartLoc :: a -> SrcLoc
  getStartLoc = rangeStart . getRange

  getEndLoc :: a -> SrcLoc
  getEndLoc = rangeEnd . getRange

instance HasRange SrcRange where
  getRange = id

instance HasRange Void where
  getRange = absurd

instance (HasRange a, HasRange b) => HasRange (Either a b) where
  getRange = either getRange getRange
  getStartLoc = either getStartLoc getStartLoc
  getEndLoc = either getEndLoc getEndLoc

instance (Generic a, GHasRange (Rep a)) => HasRange (Generically a) where
  getRange (Generically a) = getRangeRep (from a)

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
