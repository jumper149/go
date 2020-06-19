{-# LANGUAGE FlexibleContexts, TypeFamilies, UndecidableInstances #-}

module ServerState ( MonadServerState (..)
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
import Go.Game.State

data ServerState b c n = ServerState { clientsTVar' :: TVar (Clients n)
                                     , gameStateTVar' :: TVar (GameState b c n)
                                     , gameConfig' :: Config
                                     }
  deriving (Eq, Generic)

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
  clientsTVar = ServerStateT $ reader clientsTVar'
  gameStateTVar = ServerStateT $ reader gameStateTVar'
  gameConfig = ServerStateT $ reader gameConfig'

evalServerStateT :: ServerState b c n
                 -> ServerStateT b c n m a
                 -> m a
evalServerStateT ss = flip runReaderT ss . unwrapServerStateT

-- TODO: add exec-, eval- and with-functions
runNewServerStateT :: (Game b c n, MonadBase IO m)
                   => Config
                   -> ServerStateT b c n m a
                   -> m (a, GameState b c n)
runNewServerStateT config sst = do clientsTVar' <- liftBase $ newTVarIO mempty
                                   gameStateTVar' <- liftBase . newTVarIO $ either undefined id $ configure config initState -- TODO: Use RulesetEnvT
                                   let ss = ServerState { clientsTVar' = clientsTVar'
                                                        , gameStateTVar' = gameStateTVar'
                                                        , gameConfig' = config
                                                        }
                                   val <- evalServerStateT ss sst
                                   gs <- liftBase $ readTVarIO gameStateTVar'
                                   return (val , gs)
