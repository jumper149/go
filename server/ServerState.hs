{-# LANGUAGE UndecidableInstances #-}

module ServerState ( ServerStateT
                   , runServerStateT
                   , runNewServerStateT
                   ) where

import Control.Monad.Base
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Trans.Control.Functor
import Control.Monad.Trans.Control.Identity
import Control.Monad.Trans.Reader
import GHC.Conc
import GHC.Generics

import Clients
import ServerState.Class
import ServerState.Internal

import Go.Game.Game
import Go.Game.State

newtype ServerStateT m a = ServerStateT { unwrapServerStateT :: ReaderT (TVar GameSets) (ClientsT m) a }
  deriving (Applicative, Functor, Generic, Monad, MonadClients)

instance MonadTrans ServerStateT where
  lift = ServerStateT . lift . lift

instance MonadTransControl ServerStateT where
  type StT ServerStateT a = StT (ReaderT (TVar GameSets)) (StT ClientsT a)
  liftWith = defaultLiftWith2 ServerStateT unwrapServerStateT
  restoreT = defaultRestoreT2 ServerStateT

instance MonadTransControlIdentity ServerStateT where
  liftWithIdentity = defaultLiftWithIdentity

instance MonadTransFunctor ServerStateT where
  liftMap f = ServerStateT . liftMap (liftMap f) . unwrapServerStateT

instance MonadBase base m => MonadBase base (ServerStateT m) where
  liftBase = liftBaseDefault

instance MonadBaseControl base m => MonadBaseControl base (ServerStateT m) where
  type StM (ServerStateT m) a = ComposeSt ServerStateT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM

instance MonadBaseControlIdentity base m => MonadBaseControlIdentity base (ServerStateT m) where
  liftBaseWithIdentity = defaultLiftBaseWithIdentity

instance Monad m => MonadServerState (ServerStateT m) where
  gameSetsTVar = ServerStateT ask

runServerStateT :: TVar Clients
                -> TVar GameSets
                -> ServerStateT m a
                -> m a
runServerStateT cs ss = runClientsT cs . flip runReaderT ss . unwrapServerStateT

runNewServerStateT :: MonadBase IO m
                   => ServerStateT m a
                   -> m (a, Clients, GameSets)
runNewServerStateT sst = do clientsTVar' <- liftBase $ newTVarIO mempty
                            gameSetsTVar' <- liftBase $ newTVarIO mempty
                            val <- runServerStateT clientsTVar' gameSetsTVar' sst
                            cs <- liftBase $ readTVarIO clientsTVar'
                            gss <- liftBase $ readTVarIO gameSetsTVar'
                            return (val,cs,gss)
