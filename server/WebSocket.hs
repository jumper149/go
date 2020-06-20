{-# LANGUAGE FlexibleContexts #-}

module WebSocket ( websocketMiddleware
                 ) where

import Control.Exception (bracket)
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Monad.Trans.Control.Identity
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as C8 (putStrLn)
import Data.Foldable (traverse_)
import GHC.Conc
import Network.WebSockets
import Network.Wai.Handler.WebSockets.Trans
import Network.Wai.Trans (MiddlewareT)

import Clients.Class
import Message
import ServerState
import ServerState.Class

import Go.Run.JSON

websocketMiddleware :: (JSONGame b c n, MonadBaseControlIdentity IO m)
                    => MiddlewareT (ServerStateT b c n m)
websocketMiddleware = websocketsOrT defaultConnectionOptions serverApp

-- TODO: maybe ping every 30 seconds to keep alive?
serverApp :: (JSONGame b c n, MonadBaseControl IO m)
          => ServerAppT (ServerStateT b c n m)
serverApp pendingConnection = liftedBracket connect disconnect hold
    where connect = do conn <- liftBase $ acceptRequest pendingConnection
                       transact $ addClient conn
          hold key = do serverSendMessage key $ Right . ServerMessageGameState <$> readServerGameState
                        serverLoopGame key
          disconnect key = transact $ removeClient key

-- | A loop, that receives messages via the websocket and also answers back, while progressing the
-- 'GameState'.
serverLoopGame :: forall b c m n. (JSONGame b c n, MonadBase IO m)
               => ClientId
               -> ServerStateT b c n m ()
serverLoopGame key = do msg <- serverReceiveMessage key
                        liftBase . C8.putStrLn $ encode msg -- TODO: remove?
                        case msg of
                          ClientMessageFail _ -> liftBase $ putStrLn "failed to do action" -- TODO: server side error log would be better
                          ClientMessageAction action -> serverBroadcastMessage key $ fmap ServerMessageGameState <$> serverUpdateGameState key action
                          ClientMessagePlayer mbP -> let msg = Right . ServerMessagePlayer <$> updatePlayer key mbP
                                                      in serverSendMessage key msg
                        serverLoopGame key

-- | Read a message from the websocket.
serverReceiveMessage :: (JSONGame b c n, MonadBase IO m)
                     => ClientId
                     -> ServerStateT b c n m (ClientMessage b c n)
serverReceiveMessage key = do conn <- connection <$> transact (getClient key)
                              unwrapWSClientMessage <$> liftBase (receiveData conn)

-- | Send a message via the websocket.
serverSendMessage :: forall b c m n. (JSONGame b c n, MonadBase IO m)
                  => ClientId
                  -> ServerStateT b c n STM (Either String (ServerMessage b c n))
                  -> ServerStateT b c n m ()
serverSendMessage key msgSTM = do (conn , msg) <- transact $ do
                                    conn <- connection <$> getClient key
                                    msg <- msgSTM
                                    return (conn , msg)
                                  case msg of
                                    Right rMsg -> liftBase $ sendTextData conn $ WSServerMessage rMsg
                                    Left lString -> liftBase $ sendTextData conn $ WSServerMessage (ServerMessageFail lString :: ServerMessage b c n)

-- | Send a message to all clients in 'Clients' via the websocket.
serverBroadcastMessage :: forall b c m n. (JSONGame b c n, MonadBase IO m)
                       => ClientId
                       -> ServerStateT b c n STM (Either String (ServerMessage b c n))
                       -> ServerStateT b c n m ()
serverBroadcastMessage key msgSTM = do (conns , msg , conn) <- transact $ do
                                         conns <- map (connection . snd) <$> clientList
                                         msg <- msgSTM
                                         conn <- connection <$> getClient key
                                         return (conns , msg , conn)
                                       case msg of
                                         Right rMsg -> traverse_ (\ c -> liftBase . sendTextData c $ WSServerMessage rMsg) conns
                                         Left lString -> liftBase . sendTextData conn $ WSServerMessage (ServerMessageFail lString :: ServerMessage b c n)

liftedBracket :: MonadBaseControl IO m
              => m a
              -> (a -> m b)
              -> (a -> m c)
              -> m c
liftedBracket before after body = control $ \ runInBase ->
                                    bracket (runInBase before)
                                    (\ saved -> runInBase (after =<< restoreM saved))
                                    (\ saved -> runInBase (body =<< restoreM saved))
