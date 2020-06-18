{-# LANGUAGE FlexibleContexts, RecordWildCards, TypeFamilies, UndecidableInstances #-}

module ServerState ( MonadServerState (..)
                   , ServerState
                   , ServerStateT
                   , evalServerStateT
                   , runNewServerStateT
                   , transact
                   , readServerClients
                   , readServerGameState
                   , writeServerClients
                   , writeServerGameState
                   , serverAddClient
                   , serverRemoveClient
                   , serverUpdateGameState
                   , serverUpdatePlayer
                   ) where

import Control.Monad.Base
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Trans.Control.Identity
import Control.Monad.Trans.Reader
import GHC.Conc
import GHC.Generics
import Network.WebSockets (Connection)

import Client
import ServerState.Class

import Go.Game.Config
import Go.Game.Game
import Go.Game.Player
import Go.Game.Playing
import Go.Game.State

newtype ServerStateT b c n m a = ServerStateT { unwrapServerStateT :: ReaderT (ServerState b c n) m a }
  deriving (Applicative, Functor, Generic, Monad, MonadTrans, MonadTransControl, MonadTransControlIdentity, MonadTransFunctor)

instance MonadBase base m => MonadBase base (ServerStateT b c n m) where
  liftBase = liftBaseDefault

instance MonadBaseControl base m => MonadBaseControl base (ServerStateT b c n m) where
  type StM (ServerStateT b c n m) a = ComposeSt (ServerStateT b c n) m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM

instance MonadBaseControlIdentity base m => MonadBaseControlIdentity base (ServerStateT b c n m) where
  liftBaseWithIdentity inner = ServerStateT $ liftBaseWithIdentity $ \ run -> inner $ run . unwrapServerStateT

instance Monad m => MonadServerState b c n (ServerStateT b c n m) where
  serverState = ServerStateT ask

evalServerStateT :: ServerState b c n
                 -> ServerStateT b c n m a
                 -> m a
evalServerStateT ss = flip runReaderT ss . unwrapServerStateT

-- TODO: add exec-, eval- and with-functions
runNewServerStateT :: (Game b c n, MonadBase IO m)
                   => Config
                   -> ServerStateT b c n m a
                   -> m (a, GameState b c n)
runNewServerStateT gameConfig sst = do gameStateTVar <- liftBase . newTVarIO $ either undefined id $ configure gameConfig initState -- TODO: Use RulesetEnvT
                                       clientsTVar <- liftBase $ newTVarIO mempty
                                       val <- evalServerStateT ServerState {..} sst
                                       gs <- liftBase $ readTVarIO gameStateTVar
                                       return (val , gs)
