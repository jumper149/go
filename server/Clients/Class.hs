{-# LANGUAGE FlexibleContexts, FunctionalDependencies, UndecidableInstances #-}

module Clients.Class ( MonadClients (..)
                     , Clients
                     , Client (connection, identification, maybePlayer)
                     , ClientId
                     , transact
                     , readClients
                     , writeClients
                     , addClient
                     , getClient
                     , removeClient
                     , updatePlayer
                     , clientList
                     ) where

import Control.Monad.Base
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Trans.Control.Identity
import Control.Monad.Trans.Reader
import GHC.Conc
import GHC.Generics
import Network.WebSockets (Connection)

import Clients.Internal

import Go.Game.Player

class Monad m => MonadClients n m | m -> n where

  clientsTVar :: m (TVar (Clients n))

instance MonadClients n m => MonadClients n (ReaderT r m) where
  clientsTVar = lift clientsTVar

readClients :: (MonadClients n (t STM), MonadTrans t)
            => t STM (Clients n)
readClients = lift . readTVar =<< clientsTVar

writeClients :: (MonadClients n (t STM), MonadTrans t)
             => Clients n
             -> t STM ()
writeClients cs = lift . flip writeTVar cs =<< clientsTVar

transact :: (MonadBase IO m, MonadTransFunctor t)
         => t STM a
         -> t m a
transact = mapT $ liftBase . atomically

-- | Create a new client in 'Clients' with the given 'Connection'.
addClient :: (MonadClients n (t STM), MonadTrans t)
          => Connection
          -> t STM ClientId
addClient conn = do clients <- readClients
                    let client = newClientFor conn clients
                    writeClients $ addClientTo client clients
                    return $ identification client

-- | Read a specific 'Client' from 'Clients'
getClient :: (MonadClients n (t STM), MonadTrans t)
          => ClientId
          -> t STM (Client n)
getClient key = getClientFrom key <$> readClients

-- | Remove a client from 'Clients'.
removeClient :: (MonadClients n (t STM), MonadTrans t)
             => ClientId
             -> t STM ()
removeClient key = do clients <- readClients
                      writeClients $ removeClientFrom key clients

-- | Update the 'maybePlayer' of a specific 'Client' in 'STM'.
updatePlayer :: (MonadClients n (t STM), MonadTrans t)
             => ClientId
             -> Maybe (PlayerN n)
             -> t STM (Maybe (PlayerN n))
updatePlayer key mbP = do clients <- readClients
                          let client = getClientFrom key clients
                          writeClients $ addClientTo client { maybePlayer = mbP } clients
                          maybePlayer . getClientFrom key <$> readClients -- TODO: This is the same as 'return mbP'. Change?

clientList :: (MonadClients n (t STM), MonadTrans t)
           => t STM [(ClientId, Client n)]
clientList = clientListFrom <$> readClients
