{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module AlgST.Driver.Sources
  ( -- * Monad transformer
    SourcesIO,
    SourcesT,
    runSourcesT,

    -- * Accessing the @SourceManager@
    askSourceManager,
    asksSourceManager,

    -- * Adding buffers
    addBuffer,
    addFile,
    addStdin,

    -- * Handling diagnostics
    buildBaseDiagnostic,
  )
where

import AlgST.Util.Diagnose qualified as D
import AlgST.Util.SourceManager qualified as SM
import Control.Applicative
import Control.Monad.Cont.Class
import Control.Monad.Error.Class
import Control.Monad.Fix
import Control.Monad.IO.Unlift
import Control.Monad.RWS.Class
import Control.Monad.Reader
import Data.Foldable
import Data.IORef

type SourcesIO = SourcesT IO

data SourcesSt = SourcesSt
  { sourcesManager :: !SM.SourceManager,
    sourcesBaseDiag :: !D.BaseDiagnostic
  }

newtype SourcesT m a = SourcesT {unSourcesT :: ReaderT (IORef SourcesSt) m a}
  deriving newtype (Functor, Applicative, Alternative, Monad)
  deriving newtype (MonadIO, MonadFail, MonadFix)
  deriving newtype (MonadWriter w, MonadState s, MonadError e, MonadCont)

instance (MonadReader r m) => MonadReader r (SourcesT m) where
  ask = SourcesT $ lift ask
  local f (SourcesT m) = SourcesT $ ReaderT $ local f . runReaderT m

instance MonadTrans SourcesT where
  lift = SourcesT . lift

instance (MonadUnliftIO m) => MonadUnliftIO (SourcesT m) where
  withRunInIO inner = SourcesT $ withRunInIO \run -> inner (run . unSourcesT)

runSourcesT :: (MonadIO m) => SourcesT m a -> m a
runSourcesT (SourcesT m) = do
  sources <-
    liftIO . newIORef $
      SourcesSt
        { sourcesManager = SM.emptyManager,
          sourcesBaseDiag = mempty
        }
  runReaderT m sources

-- | Returns the current 'SM.SourceManager'.
--
-- The 'SM.SourceManager' may already be outdated by the time this function
-- returns.
askSourceManager :: (MonadIO m) => SourcesT m SM.SourceManager
askSourceManager = SourcesT $ ask >>= liftIO . fmap sourcesManager . readIORef

-- | Returns a value @a@ extracted from the current 'SM.SourceManager'.
--
-- The return value may already be outdated by the time this function returns.
asksSourceManager :: (MonadIO m) => (SM.SourceManager -> a) -> SourcesT m a
asksSourceManager f = f <$> askSourceManager

-- | Adds a 'SM.Buffer' to the 'SM.SourceManager'.
addBuffer :: (MonadIO m) => SM.Buffer -> SourcesT m ()
addBuffer b = SourcesT do
  ask >>= \ref -> liftIO $ atomicModifyIORef'1 ref \st ->
    SourcesSt
      { sourcesManager = SM.insertBuffer b (sourcesManager st),
        sourcesBaseDiag = D.addBuffer b (sourcesBaseDiag st)
      }

-- | Creates a new 'SM.Buffer' from an 'IO' action, adds it via 'addBuffer' and
-- returns it.
--
-- Private helper function for 'addFile' and 'addStdin'.
addBuffer' :: (MonadIO m) => IO SM.Buffer -> SourcesT m SM.Buffer
addBuffer' bufM = do
  buf <- liftIO bufM
  addBuffer buf
  pure buf

-- | Convenience function combining @"AlgST.Util.SourceManager".'SM.readFile'@
-- and 'addBuffer'.
addFile :: (MonadIO m) => FilePath -> SourcesT m SM.Buffer
addFile = addBuffer' . SM.readFile

-- | Convenience function combining @"AlgST.Util.SourceManager".'SM.readStdin'@
-- and 'addBuffer'.
addStdin :: (MonadIO m) => SourcesT m SM.Buffer
addStdin = addBuffer' $ SM.readStdin "«stdin»"

-- | Uses the buffers registered witht the current 'SM.SourceManager' to build
-- a 'D.BaseDiangostic' from a bunch of 'D.Diagnostic's.
--
-- The diagnostics are ordered by their source location before being added to
-- the 'D.BaseDiagnostic'. If the 'D.Diagnostic's come from different files the
-- final ordering may change between runs of the program because the order then
-- depends on the where the buffers are allocated on the heap.
buildBaseDiagnostic :: (MonadIO m, Foldable f) => f D.Diagnostic -> SourcesT m D.BaseDiagnostic
buildBaseDiagnostic diags = SourcesT $ ask >>= liftIO . fmap build . readIORef
  where
    build st =
      D.addSorted (sourcesManager st) (toList diags) (sourcesBaseDiag st)

-- | Like 'atomicModifyIORef'' but without the ability to return a value from
-- the inner computation.
atomicModifyIORef'1 :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef'1 ref f = atomicModifyIORef' ref \a -> (f a, ())
