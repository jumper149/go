{-# LANGUAGE FlexibleContexts, KindSignatures, OverloadedStrings, RecordWildCards #-}

module Handler where

import Control.Concurrent.MVar
import Control.Exception (finally)
import Control.Monad.Reader
import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import Data.Default.Class
import Data.Foldable (traverse_)
import qualified Data.Map as M
import qualified Data.Text as T
import GHC.Generics
import GHC.TypeLits hiding (Text)
import Network.HTTP.Types.Status (status400)
import Network.Wai (responseLBS)
import Network.Wai.Handler.Warp (Port, run)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets
import Servant
import System.Directory (listDirectory)

import API
import Html
import Message

import Go.Game.Config
import Go.Game.Player
import Go.Game.Playing
import Go.Game.State
import Go.Run.JSON

data Client n = Client { player :: Maybe (PlayerN n)
                       , connection :: Connection
                       }
  deriving Generic

type Clients n = M.Map Int (Client n)

data ServerState b c n = ServerState { gameStateMVar :: MVar (GameState b c n)
                                     , gameConfig :: Config
                                     , clientsMVar :: MVar (Clients n)
                                     }
  deriving (Eq, Generic)

newtype HandlerM b c n a = HandlerM { unwrapHandlerM :: ReaderT (ServerState b c n) Handler a }
  deriving (Applicative, Functor, Generic, Monad, MonadIO, MonadReader (ServerState b c n))

runHandlerM :: ServerState b c n -> HandlerM b c n a -> Handler a
runHandlerM ss = flip runReaderT ss . unwrapHandlerM

handler :: forall b c n. JSONGame b c n => FilePath -> ServerT API (HandlerM b c n)
handler path = gameH :<|> wssH :<|> publicH
  where gameH :: Monad m => m GameHtml
        gameH = return GameHtml {..}
          where jsAppPath = "public/all.js" -- TODO: Use path instead of hardcoded

        wssH :: (MonadIO m, MonadReader (ServerState b c n) m) => m Application
        wssH = do gs <- asks gameStateMVar
                  gc <- asks gameConfig
                  cs <- asks clientsMVar
                  return $ websocketsOr defaultConnectionOptions (handleWSConnection gs gc cs) backupApp
          where backupApp :: Application
                backupApp _ respond = respond $ responseLBS status400 [] "Not a WebSocket request"

        publicH :: ServerT Raw m
        publicH = serveDirectoryWebApp path

-- TODO: maybe ping every 30 seconds to keep alive?
handleWSConnection :: forall b c n. JSONGame b c n => MVar (GameState b c n) -> Config -> MVar (Clients n) -> PendingConnection -> IO ()
handleWSConnection gsMVar config csMVar pendingConn = do conn <- acceptRequest pendingConn
                                                         let client = Client { player = Nothing
                                                                             , connection = conn
                                                                             }
                                                         key <- addClient client
                                                         flip finally (removeClient key) $ do
                                                           update client -- TODO: remove? and only use the update in loop
                                                           loop key
  where loop key = do maybeC <- M.lookup key <$> readMVar csMVar
                      case maybeC of
                        Nothing -> error "can't find client to key"
                        Just c -> do clientMsg <- unwrapWSClientMessage <$> receiveData (connection c) :: IO (ClientMessage b c n)
                                     BS.putStrLn $ encode clientMsg
                                     case clientMsg of
                                       ClientMessageFail -> putStrLn "failed to do action"
                                       ClientMessageAction action -> do modifyMVar_ gsMVar $ return . doTurn (rules config) action
                                                                        broadcast
                                       ClientMessagePlayer mbP -> do changePlayer key mbP
                                                                     updatePlayer key
                                     loop key

        broadcast = traverse_ update =<< readMVar csMVar

        update Client {..} = sendTextData connection . WSServerMessage . ServerMessageGameState =<< readMVar gsMVar

        updatePlayer k = do mbC <- M.lookup k <$> readMVar csMVar
                            case mbC of
                              Nothing -> error "can't find client to key"
                              Just c -> sendTextData (connection c) (WSServerMessage . ServerMessagePlayer . player $ c :: WSServerMessage b c n)

        changePlayer k mbP = do mbC <- M.lookup k <$> readMVar csMVar
                                case mbC of
                                  Nothing -> error "can't find client to key"
                                  Just c -> modifyMVar_ csMVar $ return . M.insert k (c { player = mbP })

        addClient c = modifyMVar csMVar $ return . addClient' (toEnum 0) c
          where addClient' k c cs  = case M.lookup k cs of
                                       Nothing -> (M.insert k c cs , k)
                                       Just _ -> addClient' (succ k) c cs

        removeClient k = modifyMVar_ csMVar (return . M.delete k)
