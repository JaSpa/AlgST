{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UnliftedNewtypes #-}

module AlgST.Util.SourceLocation
  ( -- * Locations
    SrcLoc (..),
    advanceLoc,

    -- * Ranges
    SrcRange ((:@<), ..),
    unsafeRangeString,

    -- * @ByteString@ interactions
    startLoc,
    fullRange,
    unsafeBasePtr,
  )
where

import Data.ByteString qualified as BS
import Data.ByteString.Internal (ByteString (..))
import Data.Coerce
import Data.Hashable
import Data.Word
import Foreign
import GHC.Foreign qualified as GHC
import GHC.ForeignPtr
import GHC.Generics (Generic)
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
-- 'SrcLoc'@ instance documenttion for more information).
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
