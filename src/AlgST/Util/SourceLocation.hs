{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UnliftedNewtypes #-}

module AlgST.Util.SourceLocation where

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

advanceLoc :: SrcLoc -> Int -> SrcLoc
advanceLoc = coerce plusPtr

data SrcRange = SrcRange {rangeStart, rangeEnd :: !SrcLoc}
  deriving stock (Eq, Ord, Generic)

instance Hashable SrcRange

pattern (:@<) :: SrcLoc -> SrcLoc -> SrcRange
pattern a :@< b = SrcRange a b

-- | Counts the number of bytes included in the range.
rangeByteSize :: SrcRange -> Int
rangeByteSize r = locPtr (rangeEnd r) `minusPtr` locPtr (rangeStart r)

startLoc :: ByteString -> SrcLoc
startLoc = SrcLoc . unsfeBasePtr

fullRange :: ByteString -> SrcRange
fullRange bs = SrcRange (startLoc bs) (startLoc bs `advanceLoc` BS.length bs)

unsafeRangeString :: SrcRange -> String
unsafeRangeString r = unsafeDupablePerformIO do
  GHC.peekCStringLen IO.utf8 (castPtr (locPtr (rangeStart r)), rangeByteSize r)

unsfeBasePtr :: ByteString -> Ptr Word8
unsfeBasePtr (PS fp offset _) = unsafeForeignPtrToPtr fp `plusPtr` offset
