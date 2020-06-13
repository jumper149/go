{-# LANGUAGE FunctionalDependencies, RecordWildCards #-}

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

  serverClients :: m (TVar (Clients n))

  serverGameState :: m (TVar (GameState b c n))

  serverGameConfig :: m Config

data ServerState b c n = ServerState { clientsTVar :: TVar (Clients n)
                                     , gameStateTVar :: TVar (GameState b c n)
                                     , gameConfig :: Config
                                     }
  deriving (Eq, Generic)

newtype ServerStateT b c n m a = ServerStateT { unwrapServerStateT :: ReaderT (ServerState b c n) m a }
  deriving (Applicative, Functor, Generic, Monad, MonadIO)

instance Monad m => MonadServerState b c n (ServerStateT b c n m) where
  serverClients = ServerStateT $ reader clientsTVar
  serverGameState = ServerStateT $ reader gameStateTVar
  serverGameConfig = ServerStateT $ reader gameConfig

evalServerStateT :: ServerState b c n -> ServerStateT b c n m a -> m a
evalServerStateT ss sst = flip runReaderT ss $ unwrapServerStateT sst

-- TODO: add exec- and eval- functions
runNewServerStateT :: (Game b c n, MonadIO m) => Config -> ServerStateT b c n m a -> m (a, GameState b c n)
runNewServerStateT gameConfig sst = do gameStateTVar <- liftIO . newTVarIO $ either undefined id $ configure gameConfig initState -- TODO: Use RulesetEnvT
                                       clientsTVar <- liftIO $ newTVarIO mempty
                                       val <- runServerStateT ServerState {..} sst
                                       gs <- liftIO $ readTVarIO gameStateTVar
                                       return (val , gs)
