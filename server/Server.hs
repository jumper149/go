{-# LANGUAGE FlexibleContexts, OverloadedStrings, TypeOperators #-}

module Server where

import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson
import qualified Data.Binary.Builder as B (fromLazyByteString)
import Data.Default.Class
import qualified Data.Text as T
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.EventSource (ServerEvent (..), eventSourceAppChan)
import Network.Wai.Handler.Warp (Port, run)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets
import Servant
import Servant.HTML.Lucid
import Servant.RawM (RawM)
import System.Directory (listDirectory)

import Html

import qualified Go.Board.Default as D
import Go.Game.Config
import Go.Game.End
import Go.Game.Game
import Go.Game.Playing
import Go.Game.State
import Go.Run.JSON

type API b c n =                                           Get '[HTML] GameHtml
            :<|> "render"                               :> Get '[JSON] (GameState b c n)
            :<|> "play"   :> ReqBody '[JSON] (Action c) :> Post '[JSON] ()
            :<|> "wss"                                  :> RawM
            :<|> "sse"                                  :> RawM
            :<|> "public"                               :> Raw

api :: Proxy (API b c n)
api = Proxy

data ServerState b c n = ServerState { mVar :: MVar (GameState b c n)
                                     , chan :: Chan ServerEvent
                                     }

updateChan :: (JSONGame b c n, MonadIO m, MonadReader (ServerState b c n) m) => m ()
updateChan = do mVarGS <- asks mVar
                chanSE <- asks chan
                liftIO $ updateChanIO mVarGS chanSE

-- TODO: remove when unnecessary
updateChanIO :: JSONGame b c n => MVar (GameState b c n) -> Chan ServerEvent -> IO ()
updateChanIO mVarGS chanSE = do gs <- readMVar mVarGS
                                let encoding = ServerEvent { eventName = pure "Update GameState"
                                                           , eventId = pure "0"
                                                           , eventData = pure . B.fromLazyByteString $ encode gs
                                                           }
                                writeChan chanSE encoding

type AppM b c n = ReaderT (ServerState b c n) Handler

handler :: forall b c n. JSONGame b c n => FilePath -> Config -> ServerT (API b c n) (AppM b c n)
handler path config = gameH :<|> renderH :<|> playH :<|> wssH :<|> sseH :<|> publicH
  where gameH :: Monad m => m GameHtml
        gameH = return GameHtml

        -- TODO: remove
        renderH :: (MonadIO m, MonadReader (ServerState b c n) m)
                => m (GameState b c n)
        renderH = liftIO . readMVar =<< asks mVar

        -- TODO: remove
        playH :: (MonadIO m, MonadReader (ServerState b c n) m)
              => Action c
              -> m ()
        playH action = do gs <- asks mVar
                          liftIO $ modifyMVar_ gs f
                          updateChan
          where f = return . doTurn (rules config) action

        wssH :: (MonadIO m, MonadReader (ServerState b c n) m) => m Application
        wssH = do gs <- asks mVar
                  event <- asks chan
                  return $ websocketsOr defaultConnectionOptions (wsApp gs event) backupApp
          where wsApp :: MVar (GameState b c n) -> Chan ServerEvent -> PendingConnection -> IO ()
                wsApp gs event pendingConn = do conn <- acceptRequest pendingConn
                                                mbAction <- decode <$> receiveData conn :: IO (Maybe (Action c))
                                                case mbAction of
                                                  Nothing -> return ()
                                                  Just action -> modifyMVar_ gs (return . doTurn (rules config) action)
                                                updateChanIO gs event -- TODO: use updateChan instead, but requires rewrite of websocketsOr

                backupApp :: Application
                backupApp _ respond = respond $ responseLBS status400 [] "Not a WebSocket request"

        sseH :: MonadReader (ServerState b c n) m => m Application
        sseH = eventSourceAppChan <$> asks chan

        publicH :: ServerT Raw m
        publicH = serveDirectoryWebApp path

server :: forall b c n. JSONGame b c n => Port -> FilePath -> IO (EndScreen b n)
server port path = do putStrLn $ "Port is: " <> show port
                      putStrLn . ("Public files are: " <>) . unwords =<< listDirectory path

                      initial <- either (error . show) id <$> runConfiguredT config initState :: IO (GameState b c n) -- TODO: error?
                      gs <- newMVar initial
                      event <- newChan

                      let app = serve api $ hoistServer api (\ r -> runReaderT r (ServerState gs event)) (handler path config)
                      run port app
                      return undefined -- TODO: undefined behaviour
  where config = def
