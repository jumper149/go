{-# LANGUAGE FlexibleContexts, OverloadedStrings, TypeOperators #-}

module Server where

import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson (encode)
import Data.Binary.Builder (fromLazyByteString)
import Data.Default.Class
import Network.Wai.EventSource (ServerEvent (..), eventSourceAppChan)
import Network.Wai.Handler.Warp (Port, run)
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
            :<|> "sse"                                  :> RawM
            :<|> "public"                               :> Raw

api :: Proxy (API b c n)
api = Proxy

data ServerState b c n = ServerState { mVar :: MVar (GameState b c n)
                                     , chan :: Chan ServerEvent
                                     }

updateChan :: (JSONGame b c n, MonadIO m, MonadReader (ServerState b c n) m) => m ()
updateChan = do gs <- liftIO . readMVar =<< asks mVar
                event <- asks chan
                let encoding = ServerEvent { eventName = pure "Update GameState"
                                           , eventId = pure "0"
                                           , eventData = pure . fromLazyByteString $ encode gs
                                           }
                liftIO $ writeChan event encoding

type AppM b c n = ReaderT (ServerState b c n) Handler

handler :: forall b c n. JSONGame b c n => FilePath -> Config -> ServerT (API b c n) (AppM b c n)
handler path config = gameH :<|> renderH :<|> playH :<|> sseH :<|> publicH
  where gameH :: Monad m => m GameHtml
        gameH = return GameHtml

        renderH :: (MonadIO m, MonadReader (ServerState b c n) m)
                => m (GameState b c n)
        renderH = liftIO . readMVar =<< asks mVar

        playH :: (MonadIO m, MonadReader (ServerState b c n) m)
              => Action c
              -> m ()
        playH action = do gs <- asks mVar
                          liftIO $ modifyMVar_ gs f
          where f = return . doTurn (rules config) action

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
