{-# LANGUAGE FunctionalDependencies, RecordWildCards #-}

--module ServerState ( MonadServerState (..)
--                   , ServerState
--                   , ServerStateT
--                   , evalServerStateT
--                   , runNewServerStateT
--                   , mapServerStateT
--                   ) where
module ServerState where

import Control.Monad.Reader
import Data.Default.Class
import GHC.Conc
import GHC.Generics

import Client

import Go.Game.Config
import Go.Game.Game
import Go.Game.State

class Monad m => MonadServerState b c n m | m -> b c n where

  serverState :: m (ServerState b c n)

  serverClients :: m (TVar (Clients n))
  serverClients = clientsTVar <$> serverState

  serverGameState :: m (TVar (GameState b c n))
  serverGameState = gameStateTVar <$> serverState

  serverGameConfig :: m Config
  serverGameConfig = gameConfig <$> serverState

data ServerState b c n = ServerState { clientsTVar :: TVar (Clients n)
                                     , gameStateTVar :: TVar (GameState b c n)
                                     , gameConfig :: Config
                                     }
  deriving (Eq, Generic)

newtype ServerStateT b c n m a = ServerStateT { unwrapServerStateT :: ReaderT (ServerState b c n) m a }
  deriving (Applicative, Functor, Generic, Monad, MonadIO, MonadTrans)

instance Monad m => MonadServerState b c n (ServerStateT b c n m) where
  serverState = ServerStateT ask

evalServerStateT :: ServerState b c n -> ServerStateT b c n m a -> m a
evalServerStateT ss sst = flip runReaderT ss $ unwrapServerStateT sst

-- TODO: add exec-, eval- and with-functions
runNewServerStateT :: (Game b c n, MonadIO m) => Config -> ServerStateT b c n m a -> m (a, GameState b c n)
runNewServerStateT gameConfig sst = do gameStateTVar <- liftIO . newTVarIO $ either undefined id $ configure gameConfig initState -- TODO: Use RulesetEnvT
                                       clientsTVar <- liftIO $ newTVarIO mempty
                                       val <- evalServerStateT ServerState {..} sst
                                       gs <- liftIO $ readTVarIO gameStateTVar
                                       return (val , gs)

mapServerStateT :: (m1 a1 -> m2 a2) -> ServerStateT b c n m1 a1 -> ServerStateT b c n m2 a2
mapServerStateT f sst = ServerStateT . mapReaderT f $ unwrapServerStateT sst

readServerClients :: ServerStateT b c n STM (Clients n)
readServerClients = lift . readTVar =<< serverClients

readServerGameState :: ServerStateT b c n STM (GameState b c n)
readServerGameState = lift . readTVar =<< serverGameState

writeServerClients :: Clients n -> ServerStateT b c n STM ()
writeServerClients cs = lift . flip writeTVar cs =<< serverClients

writeServerGameState :: GameState b c n -> ServerStateT b c n STM ()
writeServerGameState gs = lift . flip writeTVar gs =<< serverGameState
