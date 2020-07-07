module Clients.Class ( MonadClients (..)
                     , Clients
                     , Client (connection, identification)
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

readClients :: (MonadBase STM m, MonadClients m)
            => m Clients
readClients = liftBase . readTVar =<< clientsTVar

writeClients :: (MonadBase STM m, MonadClients m)
             => Clients
             -> m ()
writeClients cs = liftBase . flip writeTVar cs =<< clientsTVar

-- | Create a new client in 'Clients' with the given 'Connection'.
addClient :: (MonadBase STM m, MonadClients m)
          => Connection
          -> m ClientId
addClient conn = do clients <- readClients
                    let client = newClientFor conn clients
                    writeClients $ addClientTo client clients
                    return $ identification client

-- | Read a specific 'Client' from 'Clients'.
getClient :: (MonadBase STM m, MonadClients m)
          => ClientId
          -> m Client
getClient key = getClientFrom key <$> readClients

-- | Remove a client from 'Clients'.
removeClient :: (MonadBase STM m, MonadClients m)
             => ClientId
             -> m ()
removeClient key = do clients <- readClients
                      writeClients $ removeClientFrom key clients

clientList :: (MonadBase STM m, MonadClients m)
           => m [(ClientId, Client)]
clientList = clientListFrom <$> readClients

transact :: (MonadBase IO m, MonadTransFunctor t)
         => t STM a
         -> t m a
transact = mapT $ liftBase . atomically
