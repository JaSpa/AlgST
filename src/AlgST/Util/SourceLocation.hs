{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module AlgST.Util.SourceLocation
  ( -- * Locations
    SrcLoc (..),
    advanceLoc,

    -- * Ranges
    SrcRange ((:@<), ..),
    unsafeRangeString,

    -- * Extracting ranges
    HasRange (..),
    Generically (..),
    GHasRange,

    -- * @ByteString@ interactions
    startLoc,
    fullRange,
    unsafeBasePtr,
  )
where

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
import System.IO qualified as IO
import System.IO.Unsafe

newtype SrcLoc = SrcLoc {locPtr :: Ptr Word8}
  deriving newtype (Eq, Ord, Hashable)

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
  deriving stock (Eq, Ord, Generic)

-- | This instance is only meant for debugging purposes. The output
-- unconditionally contains colorizing escape sequences (see the @Show
-- 'SrcLoc'@ instance documentation for more information).
instance Show SrcRange where
  showsPrec p r =
    showParen (p > 10) do
      showString "SrcRange "
      . coloredPtr (locPtr $ rangeStart r)
      . showChar ' '
      . showsPrec 10 (rangeByteSize r)

instance Hashable SrcRange

pattern (:@<) :: SrcLoc -> SrcLoc -> SrcRange
pattern a :@< b = SrcRange a b

{-# COMPLETE (:@<) #-}

-- | Counts the number of bytes included in the range.
rangeByteSize :: SrcRange -> Int
rangeByteSize r = locPtr (rangeEnd r) `minusPtr` locPtr (rangeStart r)

startLoc :: ByteString -> SrcLoc
startLoc = SrcLoc . unsafeBasePtr

fullRange :: ByteString -> SrcRange
fullRange bs = SrcRange (startLoc bs) (startLoc bs `advanceLoc` BS.length bs)

unsafeRangeString :: SrcRange -> String
unsafeRangeString r = unsafeDupablePerformIO do
  GHC.peekCStringLen IO.utf8 (castPtr (locPtr (rangeStart r)), rangeByteSize r)

unsafeBasePtr :: ByteString -> Ptr Word8
unsafeBasePtr (PS fp offset _) = unsafeForeignPtrToPtr fp `plusPtr` offset

class HasRange a where
  {-# MINIMAL getRange | getStartLoc, getEndLoc #-}

  getRange :: a -> SrcRange
  getRange a = getStartLoc a :@< getEndLoc a

  getStartLoc :: a -> SrcLoc
  getStartLoc = rangeStart . getRange

  getEndLoc :: a -> SrcLoc
  getEndLoc = rangeEnd . getRange

instance HasRange SrcRange where
  getRange = id

instance HasRange Void where
  getRange = absurd

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
