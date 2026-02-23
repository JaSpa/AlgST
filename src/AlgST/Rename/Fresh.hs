{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module AlgST.Rename.Fresh
  ( Fresh,
    FreshT (..),
    runFresh,
    runFreshT,
    embedFresh,
    hoistFreshT,
    etaFreshT,
    currentModule,
    freshResolved,
    freshResolvedU,
    freshResolvedParams,
  )
where

import AlgST.Syntax.Decl (Params)
import AlgST.Syntax.Name
import Control.Monad.Eta
import Control.Monad.Fix
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Bitraversable
import Data.Functor.Identity

newtype FreshT m a = Fresh {unFresh :: ReaderT ModuleName (StateT ResolvedId m) a}
  deriving newtype (Functor, Applicative, Monad, MonadFix)

type Fresh = FreshT Identity

instance MonadTrans FreshT where
  lift = Fresh . lift . lift

runFreshT :: Monad m => ModuleName -> FreshT m a -> m a
runFreshT m (Fresh a) = evalStateT (runReaderT a m) firstResolvedId

runFresh :: ModuleName -> Fresh a -> a
runFresh m = runIdentity . runFreshT m

embedFresh :: Applicative m => Fresh a -> FreshT m a
embedFresh = hoistFreshT $ pure . runIdentity

hoistFreshT :: (forall x. m x -> n x) -> FreshT m a -> FreshT n a
hoistFreshT f (Fresh a) = Fresh $ ReaderT \name -> StateT \next -> do
  f $ runStateT (runReaderT a name) next

currentModule :: Monad m => FreshT m ModuleName
currentModule = Fresh ask

freshResolved :: Monad m => Name stage scope -> FreshT m (NameR scope)
freshResolved n = do
  mod <- currentModule
  Fresh $ state \ !nextId ->
    (ResolvedName (nameWritten n) mod nextId, nextResolvedId nextId)

freshResolvedU :: Monad m => Unqualified -> FreshT m (NameR scope)
freshResolvedU = freshResolved . Name emptyModuleName

freshResolvedParams :: Monad m => Params stage -> FreshT m (Params Resolved)
freshResolvedParams = traverse (bitraverse (traverse freshResolved) pure)

etaFreshT :: FreshT m a -> FreshT m a
etaFreshT = Fresh . etaReaderT . mapReaderT etaStateT . unFresh
{-# INLINE etaFreshT #-}
