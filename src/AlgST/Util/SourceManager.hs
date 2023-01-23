{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiWayIf #-}

module AlgST.Util.SourceManager
  ( -- * Locations and Ranges
    module AlgST.Util.SourceLocation,

    -- * Buffers
    Buffer (..),
    decodeBuffer,

    -- * Handling multiple buffers
    SourceManager,
    emptyManager,
    managedBuffers,

    -- ** Inserting Buffers
    insertBuffer,
    insertStdin,
    insertFile,

    -- ** Accessing Buffers
    findContainingBuffer,
    prettySrcLoc,
    prettySrcRange,
    diagnoseSrcRange,
  )
where

import AlgST.Util.SourceLocation
import Control.Monad
import Data.ByteString qualified as BS
import Data.ByteString.Internal (ByteString (..))
import Data.ByteString.Unsafe qualified as BS
import Data.Char
import Data.Maybe
import Data.Sequence (Seq ((:<|), (:|>)))
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TE
import Data.Text.IO qualified as TIO
import Data.Word
import Error.Diagnose qualified as Diagnose
import Foreign
import GHC.Foreign qualified as GHC
import System.IO qualified as IO
import System.IO.Unsafe (unsafePerformIO)

newtype SourceManager
  = -- | A @SourceManager@ consists of a list of buffers ordered by their base
    -- pointer.
    SourceManager (Seq Buffer)
  deriving stock (Show)

-- | A @Buffer@ is consists of a 'ByteString' associated with a name, usually a
-- 'FilePath'.
--
-- The buffer contents are assumed to be valid UTF-8.
data Buffer = Buffer
  { bufferName :: FilePath,
    bufferContents :: !ByteString
  }

instance Show Buffer where
  showsPrec p b = showParen (p > 10) do
    showString "Buffer "
      . shows (bufferName b)
      . showsPrec 11 (fullRange (bufferContents b))

instance HasRange Buffer where
  getRange = fullRange . bufferContents

-- | Decodes the UTF-8 encoded 'bufferContents' to a 'String'.
decodeBuffer :: Buffer -> String
decodeBuffer b = unsafePerformIO do
  BS.unsafeUseAsCStringLen (bufferContents b) (GHC.peekCStringLen IO.utf8)

-- | Finds the buffer containing the given 'SrcLoc'. It returns the buffer's
-- name and the contents split into the part before the 'SrcLoc' and the part
-- after and including the 'SrcLoc'.
findContainingBuffer ::
  SourceManager -> SrcLoc -> Maybe (FilePath, (ByteString, ByteString))
findContainingBuffer (SourceManager buffers) loc = do
  let (_, bfs) = splitBufList loc buffers
  b :<| _ <- pure bfs
  let src = bufferContents b
      len = BS.length src
      start = unsafeBasePtr src
      end = start `plusPtr` len
  guard $ locPtr loc < end
  pure (bufferName b, BS.splitAt (locPtr loc `minusPtr` start) src)

-- | Calculates the position pointing /after the last/ character.
--
-- * It counts codepoints, not grapheme clusters.
-- * Line and column indices are 1-based.
endPosition :: ByteString -> (Int, Int)
endPosition bs = (ln, col)
  where
    -- The line index is the number of '\n' chars + 1.
    !ln = 1 + BS.count _N bs

    -- The column count is the number of characters since the start of the
    -- line + 1. We split `bs` on the last `\n` or `\r` and decode it which
    -- gives us access to the number of code points.
    --
    -- TODO: Counting code points does not require a full decode.
    !col = 1 + T.length (TE.decodeUtf8With TE.lenientDecode lastLine)
    lastLine = BS.takeWhileEnd ((&&) <$> (/= _N) <*> (/= _R)) bs

diagnoseSrcRange :: SourceManager -> SrcRange -> Diagnose.Position
diagnoseSrcRange mgr range = fromMaybe fallbackLocation do
  (fp, (prefix, suffix)) <- findContainingBuffer mgr (getStartLoc range)
  let (!sln, !scol) = endPosition prefix
  let (eln, ecol) = endPosition $ BS.take (rangeByteCount range) suffix
  let !eln' = sln + eln - 1
  let !ecol' = if eln == 1 then scol + ecol - 1 else ecol
  pure $ Diagnose.Position (sln, scol) (eln', ecol') fp

fallbackLocation :: Diagnose.Position
fallbackLocation = Diagnose.Position (1, 1) (1, 1) "«unknown location»"

_N, _R :: Word8
_N = fromIntegral (ord '\n')
_R = fromIntegral (ord '\r')

-- | Transforms a 'SrcLoc' into a human-readable version.
prettySrcLoc :: SourceManager -> SrcLoc -> ShowS
prettySrcLoc mgr loc = showString fp . showChar '@' . shows ln . showChar ':' . shows col
  where
    Diagnose.Position (ln, col) _ fp = diagnoseSrcRange mgr (SizedRange loc 0)

prettySrcRange :: SourceManager -> SrcRange -> ShowS
prettySrcRange mgr r =
  showString fp . showChar '@' . showLoc s . showChar '-' . showLoc e
  where
    showLoc (ln, col) = shows ln . showChar ':' . shows col
    Diagnose.Position s e fp = diagnoseSrcRange mgr r

emptyManager :: SourceManager
emptyManager = SourceManager mempty

managedBuffers :: SourceManager -> Seq Buffer
managedBuffers (SourceManager bs) = bs

insertBuffer :: Buffer -> SourceManager -> SourceManager
insertBuffer b (SourceManager bufList) = do
  let (xs, ys) = splitBufList (getStartLoc b) bufList
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

splitBufList :: SrcLoc -> Seq Buffer -> (Seq Buffer, Seq Buffer)
splitBufList !loc = lowerBound \b -> getStartLoc b < loc

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
