{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE UnliftedNewtypes #-}

module AlgST.Util.SourceManager
  ( -- * Locations and Ranges
    SrcLoc,
    SrcRange (..),
    startLoc,
    advanceLoc,

    -- * Buffers
    Buffer (..),
    bufferStart,

    -- * Handling multiple buffers
    SourceManager,

    -- ** Inserting Buffers
    insertBuffer,
    insertStdin,
    insertFile,

    -- ** Accessing Buffers
    findContainingBuffer,
    userSrcLoc,
    prettySrcLoc,
  )
where

import Control.Monad
import Data.ByteString qualified as BS
import Data.ByteString.Internal (ByteString (..))
import Data.Char
import Data.Coerce
import Data.Hashable
import Data.Sequence (Seq ((:<|), (:|>)))
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TE
import Data.Text.IO qualified as TIO
import Data.Word
import Foreign
import Foreign.ForeignPtr.Unsafe
import GHC.Generics (Generic)

newtype SrcLoc = SrcLoc {locPtr :: Ptr Word8}
  deriving newtype (Eq, Ord, Hashable)

advanceLoc :: SrcLoc -> Int -> SrcLoc
advanceLoc = coerce plusPtr

-- | A @SrcRange start end@ represents the half open range @[start, end)@.
data SrcRange = SrcRange {rangeStart, rangeEnd :: !SrcLoc}
  deriving stock (Eq, Ord, Generic)

instance Hashable SrcRange

newtype SourceManager
  = -- | A @SourceManager@ consists of a list of buffers ordered by their base
    -- pointer.
    SourceManager (Seq Buffer)

-- | A @Buffer@ is consists of a 'ByteString' associated with a name, usually a
-- 'FilePath'.
--
-- The buffer contents are assumed to be valid UTF-8.
data Buffer = Buffer
  { bufferName :: FilePath,
    bufferContents :: !ByteString
  }

bufferStart :: Buffer -> SrcLoc
bufferStart = startLoc . bufferContents

startLoc :: ByteString -> SrcLoc
startLoc = SrcLoc . basePtr

findContainingBuffer ::
  SrcLoc -> SourceManager -> Maybe (FilePath, (ByteString, ByteString))
findContainingBuffer loc (SourceManager buffers) = do
  let (_, bfs) = splitBufList loc buffers
  b :<| _ <- pure bfs
  let src = bufferContents b
      len = BS.length src
      start = basePtr src
      end = start `plusPtr` len
  guard $ locPtr loc < end
  pure (bufferName b, BS.splitAt (locPtr loc `minusPtr` start) src)

-- | Extracts a user-presentable @(file, line, column)@ triple from a 'SrcLoc'.
userSrcLoc :: SrcLoc -> SourceManager -> Maybe (FilePath, Int, Int)
userSrcLoc loc manager = do
  (fp, (prefix, _)) <- findContainingBuffer loc manager
  -- To find the line index we count the number of preceding '\n' chars.
  let !ln = BS.count _N prefix
  -- To get the column we count the number of characters since the start of the
  -- line. To do this (and not only count bytes) we split `prefix` at the last
  -- line break, decode it back into a text and take the text's length.
  --
  -- Sadly, this counts only code points and not grapheme clusters. However,
  -- the diagnose library does not support grapheme cluster indices either.
  let colPrefix = BS.takeWhileEnd ((&&) <$> (/= _N) <*> (/= _R)) prefix
  let !col = T.length $ TE.decodeUtf8With TE.lenientDecode colPrefix
  pure (fp, ln, col)

_N, _R :: Word8
_N = fromIntegral (ord '\n')
_R = fromIntegral (ord '\r')

-- | Transforms a 'SrcLoc' into a human-readable version.
prettySrcLoc :: SrcLoc -> SourceManager -> String
prettySrcLoc loc = maybe "«unknown location»" showLoc . userSrcLoc loc
  where
    showLoc (fp, ln, col) =
      fp ++ ':' : shows ln (':' : show col)

insertBuffer :: Buffer -> SourceManager -> SourceManager
insertBuffer b (SourceManager bufList) = do
  let (xs, ys) = splitBufList (bufferStart b) bufList
  SourceManager (xs <> Seq.singleton b <> ys)

insertStdin :: String -> SourceManager -> IO (Buffer, SourceManager)
insertStdin name = insertBufferIO name do
  -- We read STDIN first into a text before encoding it back into a ByteString.
  -- The reason is that we cannot be sure that STDIN is UTF-8 encoded. While it
  -- is reasonable to expect that from files, STDIN encoding depends on the
  -- system, the used terminal, etc.
  --
  -- TODO: Inspect STDIN's encoding and locale settings and read it directly
  -- into a bytestring if possible.
  contents <- TIO.getContents
  pure $ TE.encodeUtf8 contents

-- | Reads the file at the given path. It is assumed to contain valid UTF-8
-- encoded data.
insertFile :: FilePath -> SourceManager -> IO (Buffer, SourceManager)
insertFile fp = insertBufferIO fp (BS.readFile fp)

insertBufferIO :: FilePath -> IO ByteString -> SourceManager -> IO (Buffer, SourceManager)
insertBufferIO name readContents manager = do
  !b <- Buffer name <$> readContents
  let !manager' = insertBuffer b manager
  pure (b, manager')

basePtr :: ByteString -> Ptr Word8
basePtr (PS fp offset _) = unsafeForeignPtrToPtr fp `plusPtr` offset

splitBufList :: SrcLoc -> Seq Buffer -> (Seq Buffer, Seq Buffer)
splitBufList !loc = lowerBound \b -> bufferStart b < loc

-- | Splits a 'Seq' according to some predicate. It assumes that the sequence
-- is sorted according to the predicate.
lowerBound :: (a -> Bool) -> Seq a -> (Seq a, Seq a)
lowerBound lt as0 = go (length as0) mempty as0 mempty
  where
    go 0 leftCtxt _ rightCtxt = (leftCtxt, rightCtxt)
    go !n leftCtxt as rightCtxt = do
      let step = n `div` 2
      -- `yys` contains at least one element.
      let (xs, yys) = Seq.splitAt step as
      -- Check on which side of the split that element lies.
      if
          | y :<| ys <- yys,
            lt y ->
            -- `y` is less than the the element we are looking for.
            -- Continue into `ys`.
            go (n - step - 1) (leftCtxt <> xs :|> y) ys rightCtxt
          | otherwise ->
            -- `y` lies on the right of the split.
            -- Continue into `xs`.
            go step leftCtxt xs (yys <> rightCtxt)
