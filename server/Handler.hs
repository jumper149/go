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

                                    liftIO $ removeClientAfter csTVar key $ do
                                        sendMessage csTVar key $ Right . ServerMessageGameState <$> readTVar gsTVar
                                        loopGame gsTVar gameConfig csTVar key

serverAddClient :: Connection -> ServerStateT b c n STM ClientId
serverAddClient conn = do clients <- readServerClients
                          let client = newClient conn clients
                          writeServerClients $ addClient client clients
                          return $ identification client

serverRemoveClient :: ClientId -> ServerStateT b c n STM ()
serverRemoveClient key = do clients <- readServerClients
                            writeServerClients $ removeClient key clients

serverReceiveMessage :: JSONGame b c n => ClientId -> ServerStateT b c n IO (ClientMessage b c n)
serverReceiveMessage key = do conn <- connection . getClient key <$> mapServerStateT atomically readServerClients
                              unwrapWSClientMessage <$> liftIO (receiveData conn)

serverSendMessage :: forall b c n. JSONGame b c n => ClientId -> ServerStateT b c n STM (Either String (ServerMessage b c n)) -> ServerStateT b c n IO ()
serverSendMessage key msgSTM = do (conn , msg) <- mapServerStateT atomically $ do
                                    conn <- connection . getClient key <$> readServerClients
                                    msg <- msgSTM
                                    return (conn , msg)
                                  case msg of
                                    Right rMsg -> liftIO $ sendTextData conn $ WSServerMessage rMsg
                                    Left lString -> liftIO $ sendTextData conn $ WSServerMessage (ServerMessageFail lString :: ServerMessage b c n)

serverBroadcastMessage :: forall b c n. JSONGame b c n => TVar (Clients n) -> ClientId -> ServerStateT b c n STM (Either String (ServerMessage b c n)) -> ServerStateT b c n IO ()
serverBroadcastMessage csTVar key msgSTM = do (conns , msg , conn) <- mapServerStateT atomically $ do
                                                conns <- map (connection . snd) . toClientList <$> readServerClients
                                                msg <- msgSTM
                                                conn <- connection . getClient key <$> readServerClients
                                                return (conns , msg , conn)
                                              case msg of
                                                Right rMsg -> traverse_ (\ c -> liftIO . sendTextData c $ WSServerMessage rMsg) conns
                                                Left lString -> liftIO . sendTextData conn $ WSServerMessage (ServerMessageFail lString :: ServerMessage b c n)

serverUpdateGameState :: Game b c n => ClientId -> Action c -> ServerStateT b c n STM (Either String (GameState b c n))
serverUpdateGameState key action = do clientIsCurrent <- serverIsCurrentPlayer key
                                      if clientIsCurrent
                                         then do gs <- doTurn <$> (rules <$> serverGameConfig) <*> pure action <*> readServerGameState
                                                 writeServerGameState gs
                                                 return $ Right gs
                                         else return $ Left "it's not your turn"

serverUpdatePlayer :: ClientId -> Maybe (PlayerN n) -> ServerStateT b c n STM (Maybe (PlayerN n))
serverUpdatePlayer key mbP = do clients <- readServerClients
                                let client = getClient key clients
                                writeServerClients $ addClient client { maybePlayer = mbP } clients
                                maybePlayer . getClient key <$> readServerClients -- TODO: This is the same as 'return mbP'. Change?

serverIsCurrentPlayer :: ClientId -> ServerStateT b c n STM Bool
serverIsCurrentPlayer key = do client <- getClient key <$> readServerClients
                               gs <- readServerGameState
                               case maybePlayer client of
                                 Nothing -> return False
                                 Just p -> return $ p == currentPlayer gs

-- | Remove client from 'Clients' after the given action in 'IO' ends. Exceptions in 'IO', will be
-- caught by this function.
removeClientAfter :: TVar (Clients n) -> ClientId -> IO a -> IO a
removeClientAfter csTVar key cont = finally cont $ atomically remove
  where remove = do clients <- readTVar csTVar
                    writeTVar csTVar $ removeClient key clients

-- | A loop, that receives messages via the websocket and also answers back, while progressing the
-- 'GameState'.
loopGame :: forall b c n. JSONGame b c n => TVar (GameState b c n) -> Config -> TVar (Clients n) -> ClientId -> IO ()
loopGame gsTVar config csTVar key = do msg <- receiveMessage csTVar key :: IO (ClientMessage b c n)
                                       BS.putStrLn $ encode msg -- TODO: remove?
                                       case msg of
                                         ClientMessageFail _ -> putStrLn "failed to do action"
                                         ClientMessageAction action -> broadcastMessage csTVar key $ fmap ServerMessageGameState <$> updateGameState gsTVar config csTVar key action
                                         ClientMessagePlayer mbP -> let msg = Right . ServerMessagePlayer <$> updatePlayer csTVar key mbP :: STM (Either String (ServerMessage b c n))
                                                                     in sendMessage csTVar key msg
                                       loopGame gsTVar config csTVar key

-- | Read a message from the websocket.
receiveMessage :: JSONGame b c n => TVar (Clients n) -> ClientId -> IO (ClientMessage b c n)
receiveMessage csTVar key = do conn <- connection . getClient key <$> readTVarIO csTVar
                               unwrapWSClientMessage <$> receiveData conn

-- | Send a message via the websocket.
sendMessage :: forall b c n. JSONGame b c n => TVar (Clients n) -> ClientId -> STM (Either String (ServerMessage b c n)) -> IO ()
sendMessage csTVar key msgSTM = do (conn , msg) <- atomically $ do
                                     conn <- connection . getClient key <$> readTVar csTVar
                                     msg <- msgSTM
                                     return (conn , msg)
                                   case msg of
                                     Right rMsg -> sendTextData conn $ WSServerMessage rMsg
                                     Left lString -> sendTextData conn $ WSServerMessage (ServerMessageFail lString :: ServerMessage b c n)

-- | Send a message to all clients in 'Clients' via the websocket.
broadcastMessage :: forall b c n. JSONGame b c n => TVar (Clients n) -> ClientId -> STM (Either String (ServerMessage b c n)) -> IO ()
broadcastMessage csTVar key msgSTM = do (conns , msg , conn) <- atomically $ do
                                          conns <- map (connection . snd) . toClientList <$> readTVar csTVar
                                          msg <- msgSTM
                                          conn <- connection . getClient key <$> readTVar csTVar
                                          return (conns , msg , conn)
                                        case msg of
                                          Right rMsg -> traverse_ (\ c -> sendTextData c $ WSServerMessage rMsg) conns
                                          Left lString -> sendTextData conn $ WSServerMessage (ServerMessageFail lString :: ServerMessage b c n)

-- | Update the 'GameState' in 'STM'.
updateGameState :: Game b c n => TVar (GameState b c n) -> Config -> TVar (Clients n) -> ClientId -> Action c -> STM (Either String (GameState b c n))
updateGameState gsTVar config csTVar key action = do clientIsCurrent <- isCurrentPlayer gsTVar csTVar key
                                                     if clientIsCurrent
                                                        then do gs <- doTurn (rules config) action <$> readTVar gsTVar
                                                                writeTVar gsTVar gs
                                                                return $ Right gs
                                                        else return $ Left "it's not your turn"

-- | Update the 'maybePlayer' of a specific 'Client' in 'STM'.
updatePlayer :: TVar (Clients n) -> ClientId -> Maybe (PlayerN n) -> STM (Maybe (PlayerN n))
updatePlayer csTVar key mbP = do clients <- readTVar csTVar
                                 let client = getClient key clients
                                 writeTVar csTVar $ addClient client { maybePlayer = mbP } clients
                                 maybePlayer . getClient key <$> readTVar csTVar -- TODO: This is the same as 'return mbP'. Change?

-- | Check if given 'ClientId' is the 'currentPlayer'.
isCurrentPlayer :: TVar (GameState b c n) -> TVar (Clients n) -> ClientId -> STM Bool
isCurrentPlayer gsTVar csTVar key = do client <- getClient key <$> readTVar csTVar
                                       gs <- readTVar gsTVar
                                       case maybePlayer client of
                                         Nothing -> return False
                                         Just p -> return $ p == currentPlayer gs
