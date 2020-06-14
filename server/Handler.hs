{-# LANGUAGE FlexibleContexts, KindSignatures, OverloadedStrings, RecordWildCards #-}

module Handler where

import Control.Exception (finally)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import Data.Foldable (traverse_)
import GHC.Conc
import Network.HTTP.Types.Status (status400)
import Network.Wai (responseLBS)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets
import Servant
import System.Directory (listDirectory)

import API
import Client
import Html
import Message
import ServerState

import Go.Game.Config
import Go.Game.Game
import Go.Game.Player
import Go.Game.Playing
import Go.Game.State
import Go.Run.JSON

type ServerStateHandler b c n = ServerStateT b c n Handler

handler :: forall b c n. JSONGame b c n => FilePath -> ServerT API (ServerStateHandler b c n)
handler path = gameH :<|> wssH :<|> publicH
  where gameH :: Monad m => m GameHtml
        gameH = return GameHtml {..}
          where jsAppPath = "public/all.js" -- TODO: Use path instead of hardcoded

        wssH :: (MonadIO m, MonadServerState b c n m) => m Application
        wssH = do ss <- serverState
                  let hoistedConnector conn = evalServerStateT ss $ handleWSConnection conn -- TODO: only works, because it's MonadReader in disguise
                  return $ websocketsOr defaultConnectionOptions hoistedConnector backupApp
          where backupApp :: Application
                backupApp _ respond = respond $ responseLBS status400 [] "Not a WebSocket request"

        publicH :: ServerT Raw m
        publicH = serveDirectoryWebApp path

-- TODO: maybe ping every 30 seconds to keep alive?
handleWSConnection :: (JSONGame b c n, MonadIO m) => PendingConnection -> ServerStateT b c n m ()
handleWSConnection pendingConn = do csTVar <- serverClients
                                    gsTVar <- serverGameState
                                    gameConfig <- serverGameConfig

                                    conn <- liftIO $ acceptRequest pendingConn
                                    key <- mapServerStateT (liftIO . atomically) $ serverAddClient conn

                                    ss <- serverState

                                    liftIO $ flip finally (atomically . evalServerStateT ss $ serverRemoveClient key) $ -- TODO: only works, because it's MonadReader in disguise
                                        evalServerStateT ss $ do -- TODO: only works, because it's MonadReader in disguise
                                            serverSendMessage key $ Right . ServerMessageGameState <$> readServerGameState
                                            serverLoopGame key

serverAddClient :: Connection -> ServerStateT b c n STM ClientId
serverAddClient conn = do clients <- readServerClients
                          let client = newClient conn clients
                          writeServerClients $ addClient client clients
                          return $ identification client

serverRemoveClient :: ClientId -> ServerStateT b c n STM ()
serverRemoveClient key = do clients <- readServerClients
                            writeServerClients $ removeClient key clients

-- | Read a message from the websocket.
serverReceiveMessage :: JSONGame b c n => ClientId -> ServerStateT b c n IO (ClientMessage b c n)
serverReceiveMessage key = do conn <- connection . getClient key <$> mapServerStateT atomically readServerClients
                              unwrapWSClientMessage <$> liftIO (receiveData conn)

-- | Send a message via the websocket.
serverSendMessage :: forall b c n. JSONGame b c n => ClientId -> ServerStateT b c n STM (Either String (ServerMessage b c n)) -> ServerStateT b c n IO ()
serverSendMessage key msgSTM = do (conn , msg) <- mapServerStateT atomically $ do
                                    conn <- connection . getClient key <$> readServerClients
                                    msg <- msgSTM
                                    return (conn , msg)
                                  case msg of
                                    Right rMsg -> liftIO $ sendTextData conn $ WSServerMessage rMsg
                                    Left lString -> liftIO $ sendTextData conn $ WSServerMessage (ServerMessageFail lString :: ServerMessage b c n)

-- | Send a message to all clients in 'Clients' via the websocket.
serverBroadcastMessage :: forall b c n. JSONGame b c n => ClientId -> ServerStateT b c n STM (Either String (ServerMessage b c n)) -> ServerStateT b c n IO ()
serverBroadcastMessage key msgSTM = do (conns , msg , conn) <- mapServerStateT atomically $ do
                                         conns <- map (connection . snd) . toClientList <$> readServerClients
                                         msg <- msgSTM
                                         conn <- connection . getClient key <$> readServerClients
                                         return (conns , msg , conn)
                                       case msg of
                                         Right rMsg -> traverse_ (\ c -> liftIO . sendTextData c $ WSServerMessage rMsg) conns
                                         Left lString -> liftIO . sendTextData conn $ WSServerMessage (ServerMessageFail lString :: ServerMessage b c n)

-- | Update the 'GameState' in 'STM'.
serverUpdateGameState :: Game b c n => ClientId -> Action c -> ServerStateT b c n STM (Either String (GameState b c n))
serverUpdateGameState key action = do clientIsCurrent <- serverIsCurrentPlayer key
                                      if clientIsCurrent
                                         then do gs <- doTurn <$> (rules <$> serverGameConfig) <*> pure action <*> readServerGameState
                                                 writeServerGameState gs
                                                 return $ Right gs
                                         else return $ Left "it's not your turn"

-- | Update the 'maybePlayer' of a specific 'Client' in 'STM'.
serverUpdatePlayer :: ClientId -> Maybe (PlayerN n) -> ServerStateT b c n STM (Maybe (PlayerN n))
serverUpdatePlayer key mbP = do clients <- readServerClients
                                let client = getClient key clients
                                writeServerClients $ addClient client { maybePlayer = mbP } clients
                                maybePlayer . getClient key <$> readServerClients -- TODO: This is the same as 'return mbP'. Change?

-- | Check if given 'ClientId' is the 'currentPlayer'.
serverIsCurrentPlayer :: ClientId -> ServerStateT b c n STM Bool
serverIsCurrentPlayer key = do client <- getClient key <$> readServerClients
                               gs <- readServerGameState
                               case maybePlayer client of
                                 Nothing -> return False
                                 Just p -> return $ p == currentPlayer gs

-- | A loop, that receives messages via the websocket and also answers back, while progressing the
-- 'GameState'.
serverLoopGame :: forall b c n. JSONGame b c n => ClientId -> ServerStateT b c n IO ()
serverLoopGame key = do msg <- serverReceiveMessage key
                        liftIO . BS.putStrLn $ encode msg -- TODO: remove?
                        case msg of
                          ClientMessageFail _ -> liftIO $ putStrLn "failed to do action"
                          ClientMessageAction action -> serverBroadcastMessage key $ fmap ServerMessageGameState <$> serverUpdateGameState key action
                          ClientMessagePlayer mbP -> let msg = Right . ServerMessagePlayer <$> serverUpdatePlayer key mbP
                                                      in serverSendMessage key msg
                        serverLoopGame key
