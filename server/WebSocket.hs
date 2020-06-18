{-# LANGUAGE FlexibleContexts #-}

module WebSocket ( handleConnection
                 ) where

import Control.Exception (finally, bracket)
import Control.Monad.Base
import Control.Monad.Trans.Control
import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import Data.Foldable (traverse_)
import GHC.Conc
import Network.WebSockets

import API
import Client
import Html
import Message
import ServerState

import Go.Run.JSON

-- TODO: maybe ping every 30 seconds to keep alive?
handleConnection :: (JSONGame b c n, MonadBase IO m, MonadBaseControl IO m)
                 => PendingConnection
                 -> ServerStateT b c n m ()
handleConnection pendingConn = liftedBracket connect disconnect hold
    where connect = do conn <- liftBase $ acceptRequest pendingConn
                       transact $ serverAddClient conn
          hold key = do serverSendMessage key $ Right . ServerMessageGameState <$> readServerGameState
                        serverLoopGame key
          disconnect key = transact $ serverRemoveClient key

-- | A loop, that receives messages via the websocket and also answers back, while progressing the
-- 'GameState'.
serverLoopGame :: forall b c m n. (JSONGame b c n, MonadBase IO m)
               => ClientId
               -> ServerStateT b c n m ()
serverLoopGame key = do msg <- serverReceiveMessage key
                        liftBase . BS.putStrLn $ encode msg -- TODO: remove?
                        case msg of
                          ClientMessageFail _ -> liftBase $ putStrLn "failed to do action" -- TODO: server side error log would be better
                          ClientMessageAction action -> serverBroadcastMessage key $ fmap ServerMessageGameState <$> serverUpdateGameState key action
                          ClientMessagePlayer mbP -> let msg = Right . ServerMessagePlayer <$> serverUpdatePlayer key mbP
                                                      in serverSendMessage key msg
                        serverLoopGame key

-- | Read a message from the websocket.
serverReceiveMessage :: (JSONGame b c n, MonadBase IO m)
                     => ClientId
                     -> ServerStateT b c n m (ClientMessage b c n)
serverReceiveMessage key = do conn <- connection . getClient key <$> transact readServerClients
                              unwrapWSClientMessage <$> liftBase (receiveData conn)

-- | Send a message via the websocket.
serverSendMessage :: forall b c m n. (JSONGame b c n, MonadBase IO m)
                  => ClientId
                  -> ServerStateT b c n STM (Either String (ServerMessage b c n))
                  -> ServerStateT b c n m ()
serverSendMessage key msgSTM = do (conn , msg) <- transact $ do
                                    conn <- connection . getClient key <$> readServerClients
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
                                         conns <- map (connection . snd) . toClientList <$> readServerClients
                                         msg <- msgSTM
                                         conn <- connection . getClient key <$> readServerClients
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
