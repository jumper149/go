{-# LANGUAGE FlexibleContexts #-}

module Clients.Class ( MonadClients (..)
                     , Clients
                     , Client (connection, identification, maybeGameId)
                     , ClientId
                     , transact
                     , readClients
                     , writeClients
                     , addClient
                     , getClient
                     , removeClient
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

class Monad m => MonadClients m where

  clientsTVar :: m (TVar Clients)

instance MonadClients m => MonadClients (ReaderT r m) where
  clientsTVar = lift clientsTVar

readClients :: (MonadClients (t STM), MonadTrans t)
            => t STM Clients
readClients = lift . readTVar =<< clientsTVar

writeClients :: (MonadClients (t STM), MonadTrans t)
             => Clients
             -> t STM ()
writeClients cs = lift . flip writeTVar cs =<< clientsTVar

transact :: (MonadBase IO m, MonadTransFunctor t)
         => t STM a
         -> t m a
transact = mapT $ liftBase . atomically

-- | Create a new client in 'Clients' with the given 'Connection'.
addClient :: (MonadClients (t STM), MonadTrans t)
          => Connection
          -> t STM ClientId
addClient conn = do clients <- readClients
                    let client = newClientFor conn clients
                    writeClients $ addClientTo client clients
                    return $ identification client

-- | Read a specific 'Client' from 'Clients'.
getClient :: (MonadClients (t STM), MonadTrans t)
          => ClientId
          -> t STM Client
getClient key = getClientFrom key <$> readClients

-- | Remove a client from 'Clients'.
removeClient :: (MonadClients (t STM), MonadTrans t)
             => ClientId
             -> t STM ()
removeClient key = do clients <- readClients
                      writeClients $ removeClientFrom key clients

clientList :: (MonadClients (t STM), MonadTrans t)
           => t STM [(ClientId, Client)]
clientList = clientListFrom <$> readClients
