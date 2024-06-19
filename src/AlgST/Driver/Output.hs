{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module AlgST.Driver.Output
  ( -- * Outputting messages

    -- ** Abstract handles
    OutputHandle,
    nullHandle,
    isNullHandle,

    -- ** Creating dynamic handles
    withOutput,
    captureOutput,
    captureOutputWithSettings,
    OutputSettings (..),
    defaultOutputSettings,

    -- ** Writing to an @OutputHandle@
    outputStr,
    outputStrLn,
    outputShow,
    outputLnS,
    outputS,
    outputDoc,
    outputError,

    -- ** Sticky messages
    -- $sticky
    outputSticky,
    clearSticky,

    -- * Sticky progress counters
    Counter,
    newCounter,
    newZeroCounter,
    newCounterStart,
    counterDone,
    CounterState (..),
    zeroCounter,
    counterRunningL,
    counterFinishedL,
    counterOverallL,
    counterTitleL,
    counterUpdate,
    wrapCounter,
  )
where

import AlgST.Util.Lenses
import Control.Category ((>>>))
import Control.Concurrent
import Control.Concurrent.Async (Async)
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM
import Control.Exception
import Control.Foldl qualified as L
import Control.Monad
import Control.Monad.IO.Unlift
import Data.Coerce
import Data.Functor
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe
import Data.Monoid
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Lens.Family2
import Prettyprinter
import Prettyprinter.Render.String
import Prettyprinter.Render.Terminal
import Prettyprinter.Render.Terminal qualified as Term
import Prettyprinter.Render.Text qualified as Text
import System.Console.ANSI qualified as ANSI
import System.Console.Terminal.Size qualified as Term
import System.IO
import System.IO.Unsafe (unsafeDupablePerformIO)

newtype Sticky = Sticky String

emptySticky :: Sticky
emptySticky = Sticky ""

data OutputSettings = OutputSettings
  { forceDoAnsi :: !(Maybe Bool),
    forceOutputWidth :: !(Maybe PageWidth)
  }
  deriving (Show)

defaultOutputSettings :: OutputSettings
defaultOutputSettings =
  OutputSettings
    { forceDoAnsi = Nothing,
      forceOutputWidth = Nothing
    }

data OutputHandle = OutputHandle
  { outputBuffer :: !ActionBuffer,
    outputWidth :: !PageWidth
  }
  deriving (Show)

-- | An output handle which discards all messages.
nullHandle :: OutputHandle
nullHandle =
  OutputHandle
    { outputBuffer = unsafeDupablePerformIO $ newActionBufferIO 0,
      outputWidth = Unbounded
    }
{-# NOINLINE nullHandle #-}

-- | Checks wether the given handle discards all messages, such as
-- 'nullHandle'.
isNullHandle :: OutputHandle -> Bool
isNullHandle OutputHandle {outputBuffer = NullBuffer} = True
isNullHandle _ = False

-- | Captures the output written to the 'OutputHandle' provided in the nested
-- computation.
--
-- The 'OutputHandle' provided to the sub-computation must not be used once the
-- subcomputation returns.
--
-- The 'OutputSettings' applied to messages written to the 'OutputHandle'
-- disable all colors and allow for an unlimited 'PageWidth'. Use
-- 'captureOutputWithSettings' to specify custom 'OutputSettings'.
captureOutput :: (MonadUnliftIO m) => (OutputHandle -> m a) -> m (String, a)
captureOutput = captureOutputWithSettings defaultOutputSettings

-- | Captures the output written to the 'OutputHandle' provided in the nested
-- computation.
--
-- The 'OutputHandle' provided to the sub-computation must not be used once the
-- it returns.
--
-- The 'OutputSettings' can be used to allow colorized output and to limit the
-- 'PageWidth' (the default is 'Unlimited'). Sticky messages are always
-- disabled.
captureOutputWithSettings :: (MonadUnliftIO m) => OutputSettings -> (OutputHandle -> m a) -> m (String, a)
captureOutputWithSettings settings =
  withAsyncOutput (pure $ forceOutputWidth settings) do
    runOutputCollect $ False `fromMaybe` forceDoAnsi settings

-- | Writes all output written to the 'OutputHandle' to the provided 'Handle'.
--
-- The 'OutputHandle' provided to the sub-computation must not be used once the
-- it returns.
withOutput :: (MonadUnliftIO m) => OutputSettings -> Handle -> (OutputHandle -> m a) -> m a
withOutput settings h = fmap snd <$> withAsyncOutput (detectOutputWidth settings h) (runOutputHandle settings h)

-- | An exception thrown by the consumer of a 'OutputHandle'.
newtype OutputException = OutputException SomeException
  deriving stock (Show)

instance Exception OutputException

{- Note [Async Consumer / Producer]

We run the consumer in a new thread, asynchronous to the producer. We want to
ensure that

(1) We wait for the consumer to terminate before returning from
    `withAsyncOutput`.

    This is also required by the type signature of `withAsyncOutput`. One part
    of the return value has to come from the completed consumer.

    It is achieved by first sending the termination signal using
    `terminateActionBuffer` and then blocking on `wait` inside of `withAsync`.

(2) An exception in the producer should terminate the consumer gracefully.

    We achieve this in the same manner as (1). We do this by wrapping the
    producer in `finally`.

(3) An exception in the consumer should propagate the exception to the
    producer.

    This is important so that a crashed consumer does not lead to a producer
    blocked on a full `OutputHandle`.

    This is the part that is tricky: if we only do a `wait` once the producer
    has finished we never get to the point to forward the exception in a way to
    interrupt the producer as it might be blocked on a full `OutputHandle`.

    We could `link` the consumer to the producer thread. This would forward any
    exceptions as soon as they originate in the consumer. How to wait for the
    consumer to terminate in the successfull case though? If we use `wait` the
    ordering between the asynchronous forwarding from `link` and the
    synchronous throw from `wait` is not guaranteed. I think it might even
    happen that the synchronous version is partially handled before suddenly
    the asynchronous exception interrupts.

    We use a custom version of `link` which is run using `withAsync` so that
    the linking is scoped. Exceptions forwarded through `withScopedLink` are
    wrapped inside `OutputException`.
-}

-- | Establishes a link from the given async to the current thread for the
-- duration of the given 'IO' computation.
withScopedLink :: Async a -> IO r -> IO r
withScopedLink target m = do
  -- Get a refernce to the thread which should be informed about exceptions.
  me <- myThreadId
  -- Disable async exceptions until we are properly set-up.
  mask $ \restore -> Async.withAsyncWithUnmask (link me) \_linker ->
    -- Run the inner computation with exceptions enabled again.
    restore m
  where
    link :: ThreadId -> (forall a. IO a -> IO a) -> IO ()
    link tid unmask = do
      -- The linker waits for the target thread to complete.
      res <- unmask $ Async.waitCatch target
      case res of
        -- An exception is forwarded to the calling thread, wrapped in
        -- `OutputException`.
        Left e -> throwTo tid (OutputException e)
        -- A successfull results indicates termination.
        Right _ -> pure ()

withAsyncOutput :: (MonadUnliftIO m) => IO (Maybe PageWidth) -> (ActionBuffer -> IO a) -> (OutputHandle -> m b) -> m (a, b)
withAsyncOutput getPageWidth consumer producer =
  -- Retrieve the necessary information to run `m` actions inside `IO`.
  askRunInIO >>= \runIO -> liftIO do
    -- Decide on the width of the terminal.
    pageWidth <- fromMaybe Unbounded <$> getPageWidth
    -- Create the output buffer. (The buffer size of 32 has been chosen
    -- semi-randomly.)
    buf <- newActionBufferIO 32
    -- Start the consumer thread.
    Async.withAsync (consumer buf) \outputThread -> do
      -- Run the producer. If the consumer raises any exceptions during that
      -- time they will be forwarded to us.
      let linkedProducer = withScopedLink outputThread do
            runIO (producer OutputHandle {outputBuffer = buf, outputWidth = pageWidth})
      b <-
        linkedProducer `finally` do
          -- Signal the consumer the end.
          atomically (terminateActionBuffer buf)
          -- Wait for the consumer to terminate.
          Async.waitCatch outputThread

      -- We only arrive here if there was no exception interrupting us. The
      -- consumer will already have completed (due to the `waitCatch` in the
      -- above `finally` block) but there we ignored potential exceptions.
      -- Unwrap the consumers result now.
      res <- Async.waitCatch outputThread
      case res of
        Left e -> throwIO (OutputException e)
        Right a -> pure (a, b)

detectOutputWidth :: OutputSettings -> Handle -> IO (Maybe PageWidth)
detectOutputWidth OutputSettings {forceOutputWidth = Just wd} _ = pure (Just wd)
detectOutputWidth _ h = fmap (\w -> AvailablePerLine (Term.width w) 1.0) <$> Term.hSize h

outputStr :: (MonadIO m) => OutputHandle -> String -> m ()
outputStr b = outputS b . showString

outputStrLn :: (MonadIO m) => OutputHandle -> String -> m ()
outputStrLn b s = outputS b $ showString s . showChar '\n'

outputShow :: (MonadIO m, Show a) => OutputHandle -> a -> m ()
outputShow b = outputLnS b . shows

outputLnS :: (MonadIO m) => OutputHandle -> ShowS -> m ()
outputLnS h s = outputS h (s . showChar '\n')

outputS :: (MonadIO m) => OutputHandle -> ShowS -> m ()
outputS OutputHandle {outputBuffer = buf} =
  liftIO . writeActionBuffer buf . WriteMessage

outputDoc :: (MonadIO m) => OutputHandle -> (PageWidth -> SimpleDocStream AnsiStyle) -> m ()
outputDoc h d = liftIO $ writeActionBuffer (outputBuffer h) $ WriteDoc $! d $ outputWidth h

outputError :: (MonadIO m) => OutputHandle -> String -> m ()
outputError h s = outputDoc h \wd ->
  layoutPretty (LayoutOptions wd) . annotate (Term.color Term.Red) $ pretty s

-- | Replaces the current sticky message with the given string.
outputSticky :: (MonadIO m) => OutputHandle -> String -> m ()
outputSticky OutputHandle {outputBuffer = buf} =
  liftIO . writeActionBuffer buf . SetSticky . Sticky

-- | Clears the current sticky message.
clearSticky :: (MonadIO m) => OutputHandle -> m ()
clearSticky h = outputSticky h ""

-- $sticky
--
-- A sticky message will stick to the bottom of the terminal: When other
-- messages are output via 'outputStr' or 'outputStrLn' the sitcky message will
-- be cleared and re-output below.
--
-- === Sticky Message Limitations ===
--
-- 1. Messages will only stick to the bottom of the screen for other
-- messages output via the same 'OutputHandle'.
--
-- 2. Sticky messages should be kept relatively short: if the terminal's width
-- broke a sticky message into multiple lines it would become impossible to
-- clear more than the last line resulting in the partial sticky message
-- remaining on screen.
--
-- 3. Support for sticky messages requires for the output device to support
-- ANSI escape sequences. If support for these is not detected sticky messages
-- will appear as if output as regular messages via 'outputStrLn'.

-- | A bounded buffer of output actions.
data ActionBuffer = ActionBuffer !Word !(TVar Word) !(TVar [Action])

instance Show ActionBuffer where
  show (ActionBuffer cap _ _) =
    "ActionBuffer {capacity = " <> show cap <> "}"

pattern NullBuffer :: ActionBuffer
pattern NullBuffer <- ActionBuffer 0 _ _

newActionBufferIO :: Word -> IO ActionBuffer
newActionBufferIO !cap =
  ActionBuffer cap <$> newTVarIO cap <*> newTVarIO []

writeActionBuffer :: ActionBuffer -> Action -> IO ()
writeActionBuffer NullBuffer _ = pure ()
writeActionBuffer (ActionBuffer _ capVar actVar) a = atomically do
  cap <- readTVar capVar
  guard $ cap > 0
  writeTVar capVar $! cap - 1
  modifyTVar actVar (a :)

terminateActionBuffer :: ActionBuffer -> STM ()
terminateActionBuffer (ActionBuffer _ _ actVar) = do
  -- This should be the last write, do it unconditionally.
  modifyTVar actVar (Done :)

-- | Retrieves the reverse list of 'Action's to be interpreted.
readActionBufferRev :: ActionBuffer -> STM (NonEmpty Action)
readActionBufferRev (ActionBuffer cap capVar actVar) = do
  actions <- readTVar actVar
  result <- case actions of
    [] -> retry
    a : as -> pure $ a :| as
  writeTVar actVar []
  writeTVar capVar cap
  pure result

-- | A 'L.Fold' to interpret a (reverse!) sequence of 'Action's into a triple
-- of @(sticky, output, done)@.
--
-- * @sticky@ is the new sticky message to write after the output.
-- * @output@ is the output to write.
-- * @done@ is 'True' if we should stop waiting for new messages.
interpretRev :: Bool -> Sticky -> L.Fold Action (Sticky, ChunkList -> ChunkList, Bool)
interpretRev !useSticky sticky0 =
  (,,)
    <$> ( if useSticky
            then mapMaybeL getSticky (fromMaybe sticky0 <$> L.head)
            else pure sticky0
        )
    <*> L.foldMap getMessage coerce
    <*> L.any isDone
  where
    getSticky = \case
      SetSticky s -> Just s
      _ -> Nothing
    getMessage = \case
      WriteDoc d -> Dual $ Endo $ ChunkD d
      WriteMessage s -> Dual $ Endo $ ChunkS s
      SetSticky (Sticky s) | not useSticky -> Dual $ Endo $ ChunkS (showString s . showChar '\n')
      _ -> mempty
    isDone = \case
      Done -> True
      _ -> False

-- | 'mapMaybe' for 'L.Fold'.
mapMaybeL :: (a -> Maybe b) -> L.Fold b c -> L.Fold a c
mapMaybeL f (L.Fold step0 ini0 final0) = L.Fold step ini0 final0
  where
    step x = maybe x (step0 x) . f

data ChunkList
  = ChunkS ShowS ChunkList
  | ChunkD (SimpleDocStream AnsiStyle) ChunkList
  | ChunkFlush

data Action
  = SetSticky Sticky
  | WriteMessage ShowS
  | WriteDoc (SimpleDocStream AnsiStyle)
  | Done

runOutputHandle :: OutputSettings -> Handle -> ActionBuffer -> IO ()
runOutputHandle settings h buf = bracket prepare id (const run)
  where
    prepare :: IO (IO ())
    prepare = do
      -- We want a block buffered handle, we flush it at the end of each chunk
      -- of output.
      buf <- hGetBuffering h
      case buf of
        BlockBuffering _ ->
          -- Handle is already block buffered but make sure it is flushed at
          -- the end.
          pure (hFlush h)
        _ ->
          -- Make it block buffered, restore the previous buffering at the end.
          hSetBuffering h (BlockBuffering Nothing)
            $> hSetBuffering h buf

    run :: IO ()
    run = do
      -- If we are allowed to use ANSI escapes and the handle looks like it
      -- supports it we enable sticky messages.
      useSticky <-
        if forceDoAnsi settings == Just False
          then pure False
          else ANSI.hNowSupportsANSI h
      useColors <- maybe (ANSI.hSupportsANSIColor h) pure (forceDoAnsi settings)
      let docRender = (if useColors then Term.renderIO else Text.renderIO) h
      runOutput (\_ -> writeChunk (clearCode useSticky) docRender) () useSticky buf

    writeChunk :: ShowS -> (SimpleDocStream AnsiStyle -> IO ()) -> ChunkList -> IO ()
    writeChunk prefix renderDoc = \case
      ChunkS s rest -> writeChunk (prefix . s) renderDoc rest
      ChunkD d rest -> hPutStr h (prefix "") *> renderDoc d *> writeChunk id renderDoc rest
      ChunkFlush -> hPutStr h (prefix "") *> hFlush h

    clearCode :: Bool -> ShowS
    clearCode True =
      -- Clear the current line which contains the current sticky or is empty.
      showString ANSI.clearLineCode . showString (ANSI.setCursorColumnCode 0)
    clearCode False =
      -- ANSI is not supported: since stickies are interleaved with normal
      -- messages nothing has to be cleared.
      id

runOutputCollect :: Bool -> ActionBuffer -> IO String
runOutputCollect !useColors buf = do
  s <- runOutput writeChunks id False buf
  pure $ s ""
  where
    writeChunks s = \case
      ChunkS c next -> writeChunks (s . c) next
      ChunkD d next -> writeChunks (s . renderDoc d) next
      ChunkFlush -> pure s
    renderDoc
      | useColors = TL.foldrChunks (\x y -> showString (T.unpack x) . y) id . Term.renderLazy
      | otherwise = renderShowS

runOutput :: forall s. (s -> ChunkList -> IO s) -> s -> Bool -> ActionBuffer -> IO s
runOutput writeChunks s0 !useSticky actBuffer = go s0 emptySticky
  where
    go :: s -> Sticky -> IO s
    go !s prevSticky = do
      -- Read the next batch.
      acts <- atomically $ readActionBufferRev actBuffer
      let (Sticky sticky, chunks, done) = L.fold (interpretRev useSticky prevSticky) acts
      -- Include a final newline if output is completed after this batch.
      let stickyFlush
            | done && not (null sticky) = ChunkS (showString sticky . showChar '\n') ChunkFlush
            | otherwise = ChunkS (showString sticky) ChunkFlush
      -- Write that batch to the output.
      s' <- writeChunks s $ chunks stickyFlush
      -- Continue with the next batch if we're not yet done.
      if done
        then pure s'
        else go s' (Sticky sticky)

data CounterState = CounterState
  { counterRunning :: !Int,
    counterFinished :: !Int,
    counterOverall :: !Int,
    counterTitle :: String
  }

data Counter = Counter !OutputHandle !(MVar CounterState)

{- ORMOLU_DISABLE -}
makeLenses ''CounterState
counterRunningL :: Lens' CounterState Int
counterFinishedL :: Lens' CounterState Int
counterOverallL :: Lens' CounterState Int
counterTitleL :: Lens' CounterState String
{- ORMOLU_ENABLE -}

-- | A 'CounterState' with all fields set to zero and an empty title.
zeroCounter :: CounterState
zeroCounter = CounterState 0 0 0 ""

-- | Creates a new counter starting from the given initial 'CounterState'.
newCounter :: (MonadIO m) => OutputHandle -> CounterState -> m Counter
newCounter handle !st = liftIO $ Counter handle <$> newMVar st

-- | Creates a new counter starting from 'zeroCounter'.
newZeroCounter :: (MonadIO m) => OutputHandle -> m Counter
newZeroCounter handle = newCounter handle zeroCounter

-- | Create a new counter starting from the given state and immediately output
-- the first message.
newCounterStart :: (MonadIO m) => OutputHandle -> CounterState -> m Counter
newCounterStart handle st = liftIO do
  cntr <- newCounter handle st
  cntr <$ outputCounter cntr st

renderCounter :: CounterState -> ShowS
renderCounter CounterState {..} =
  showChar '['
    . showPadded counterRunning
    . showChar '/'
    . showPadded counterFinished
    . showChar '/'
    . showPadded counterOverall
    . showChar ']'
    . if null counterTitle then id else showString (' ' : counterTitle)
  where
    maxlen =
      -- We assume that counters won't be negative.
      length . show $
        counterRunning `max` counterFinished `max` counterOverall
    showPadded n =
      let nlen = length $ show n
       in showString (replicate (maxlen - nlen) ' ') . shows n

outputCounter :: Counter -> CounterState -> IO ()
outputCounter (Counter handle _) st =
  outputSticky handle $ renderCounter st ""

counterUpdate :: (MonadIO m) => Counter -> (CounterState -> CounterState) -> m ()
counterUpdate cntr@(Counter _ stVar) f =
  liftIO $ modifyMVar_ stVar \st -> do
    let !st' = f st
    outputCounter cntr st'
    pure st'

counterDone :: (MonadIO m) => Counter -> m ()
counterDone (Counter handle _) = clearSticky handle

wrapCounter :: (MonadIO m) => Counter -> String -> m a -> m a
wrapCounter c title m = counterUpdate c start *> m <* counterUpdate c end
  where
    start = counterRunningL +~ 1 >>> counterTitleL .~ title
    end = counterRunningL -~ 1 >>> counterFinishedL +~ 1
