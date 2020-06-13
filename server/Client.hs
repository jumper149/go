{-# LANGUAGE RecordWildCards #-}

module Client ( ClientId
              , Client (connection, identification, maybePlayer)
              , Clients
              , newClient
              , addClient
              , getClient
              , removeClient
              , toClientList
              ) where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import GHC.Generics
import Network.WebSockets (Connection)

import Message

import Go.Game.Player

-- | An identifier for 'Client'.
newtype ClientId = ClientId { unwrapClientId :: Integer }
  deriving (Enum, Eq, Generic, Ord, Read, Show)

-- | A data type, that holds information regarding a client.
data Client n = Client { connection :: Connection
                       , identification :: ClientId
                       , maybePlayer :: Maybe (PlayerN n)
                       }
  deriving Generic

-- | A data type holding 'Client's
newtype Clients n = Clients { unwrapClients :: M.Map ClientId (Client n) }
  deriving (Generic, Semigroup, Monoid)

-- | Create a new 'Client', that can be added to 'Clients'. The client is only uniquely
-- identifiable within 'Clients'.
newClient :: Connection -> Clients n -> Client n
newClient conn (Clients cs) = Client {..}
  where connection = conn
        identification = fromMaybe (ClientId 0) $ succ . fst <$> M.lookupMax cs
        maybePlayer = Nothing

-- | Inserts a 'Client' into 'Clients'. Assumes, that the identification is not already used in
-- 'Clients', overwrites the old one otherwise.
addClient :: Client n -> Clients n -> Clients n
addClient c (Clients cs) = Clients $ M.insert (identification c) c cs

-- | Get 'Client' from 'Clients'. Fails with an exception, when no 'Client' is found.
getClient :: ClientId -> Clients n -> Client n
getClient k (Clients cs) = fromMaybe (errorClientNotFound k) $ M.lookup k cs

-- | Remove the 'Client' with the given 'ClientId' from 'Clients'. Acts like 'id', when no 'Client'
-- is found.
removeClient :: ClientId -> Clients n -> Clients n
removeClient k (Clients cs) = Clients $ M.delete k cs

toClientList :: Clients n -> [(ClientId, Client n)]
toClientList (Clients cs) = M.toList cs

errorClientNotFound :: ClientId -> a
errorClientNotFound k = error $ "Can't find client with id: " <> show k
