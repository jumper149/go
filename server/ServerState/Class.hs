{-# LANGUAGE FlexibleContexts, FunctionalDependencies #-}

module ServerState.Class ( MonadServerState (..)
                         , ServerState (..)
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
import GHC.Conc
import GHC.Generics
import Network.WebSockets (Connection)

import Client

import Go.Game.Config
import Go.Game.Game
import Go.Game.Player
import Go.Game.Playing
import Go.Game.State

class Monad m => MonadServerState b c n m | m -> b c n where

  serverState :: m (ServerState b c n)

readServerClients :: (MonadServerState b c n (t STM), MonadTrans t)
                  => t STM (Clients n)
readServerClients = lift . readTVar . clientsTVar =<< serverState

readServerGameState :: (MonadServerState b c n (t STM), MonadTrans t)
                    => t STM (GameState b c n)
readServerGameState = lift . readTVar . gameStateTVar =<< serverState

writeServerClients :: (MonadServerState b c n (t STM), MonadTrans t)
                   => Clients n
                   -> t STM ()
writeServerClients cs = lift . flip writeTVar cs . clientsTVar =<< serverState

writeServerGameState :: (MonadServerState b c n (t STM), MonadTrans t)
                     => GameState b c n
                     -> t STM ()
writeServerGameState gs = lift . flip writeTVar gs . gameStateTVar =<< serverState

serverGameConfig :: MonadServerState b c n m => m Config
serverGameConfig = gameConfig <$> serverState

data ServerState b c n = ServerState { clientsTVar :: TVar (Clients n)
                                     , gameStateTVar :: TVar (GameState b c n)
                                     , gameConfig :: Config
                                     }
  deriving (Eq, Generic)

transact :: (MonadBase IO m, MonadTransFunctor t)
         => t STM a
         -> t m a
transact = mapT $ liftBase . atomically

-- | Create a new client in 'Clients' with the given 'Connection'.
serverAddClient :: (MonadServerState b c n (t STM), MonadTrans t)
                => Connection
                -> t STM ClientId
serverAddClient conn = do clients <- readServerClients
                          let client = newClient conn clients
                          writeServerClients $ addClient client clients
                          return $ identification client

-- | Remove a client from 'Clients'.
serverRemoveClient :: (MonadServerState b c n (t STM), MonadTrans t)
                   => ClientId
                   -> t STM ()
serverRemoveClient key = do clients <- readServerClients
                            writeServerClients $ removeClient key clients

-- | Update the 'GameState' in 'STM' and return the new 'GameState'.
serverUpdateGameState :: (Game b c n, MonadServerState b c n (t STM), MonadTrans t)
                      => ClientId
                      -> Action c
                      -> t STM (Either String (GameState b c n))
serverUpdateGameState key action = do clientIsCurrent <- serverIsCurrentPlayer key
                                      if clientIsCurrent
                                         then do gs <- doTurn <$> (rules <$> serverGameConfig) <*> pure action <*> readServerGameState
                                                 writeServerGameState gs
                                                 return $ Right gs
                                         else return $ Left "it's not your turn"

-- | Update the 'maybePlayer' of a specific 'Client' in 'STM'.
serverUpdatePlayer :: (MonadServerState b c n (t STM), MonadTrans t)
                   => ClientId
                   -> Maybe (PlayerN n)
                   -> t STM (Maybe (PlayerN n))
serverUpdatePlayer key mbP = do clients <- readServerClients
                                let client = getClient key clients
                                writeServerClients $ addClient client { maybePlayer = mbP } clients
                                maybePlayer . getClient key <$> readServerClients -- TODO: This is the same as 'return mbP'. Change?

-- | Check if given 'ClientId' is the 'currentPlayer'. Helper function for 'serverUpdateGameState'.
serverIsCurrentPlayer :: (MonadServerState b c n (t STM), MonadTrans t)
                      => ClientId
                      -> t STM Bool
serverIsCurrentPlayer key = do client <- getClient key <$> readServerClients
                               gs <- readServerGameState
                               case maybePlayer client of
                                 Nothing -> return False
                                 Just p -> return $ p == currentPlayer gs
