{-# LANGUAGE FlexibleContexts, KindSignatures, OverloadedStrings, TypeOperators #-}

module Server where

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson
import qualified Data.Binary.Builder as B
import qualified Data.ByteString.Lazy as BS
import Data.Default.Class
import qualified Data.Text as T
import GHC.Generics
import GHC.TypeLits hiding (Text)
import Network.HTTP.Types.Status (status400)
import Network.Wai
import Network.Wai.EventSource (ServerEvent (..), eventSourceAppChan)
import Network.Wai.Handler.Warp (Port, run)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets
import Servant
import Servant.HTML.Lucid (HTML)
import Servant.RawM (RawM)
import System.Directory (listDirectory)

import Html
import Message

import qualified Go.Board.Default as D
import Go.Game.Config
import Go.Game.End
import Go.Game.Game
import Go.Game.Playing
import Go.Game.State
import Go.Run.JSON

type API =             Get '[HTML] GameHtml
      :<|> "wss"    :> RawM
      :<|> "public" :> Raw

api :: Proxy API
api = Proxy

newtype ServerState b c n = ServerState { gsMVar :: MVar (GameState b c n) }

type AppM b c n = ReaderT (ServerState b c n) Handler

handler :: forall b c n. JSONGame b c n => FilePath -> Config -> ServerT API (AppM b c n)
handler path config = gameH :<|> wssH :<|> publicH
  where gameH :: Monad m => m GameHtml
        gameH = return GameHtml

        wssH :: (MonadIO m, MonadReader (ServerState b c n) m) => m Application
        wssH = do gs <- asks gsMVar
                  return $ websocketsOr defaultConnectionOptions (wsApp gs) backupApp
          where wsApp :: MVar (GameState b c n) -> PendingConnection -> IO ()
                wsApp gsMVar pendingConn = do conn <- acceptRequest pendingConn
                                              gs <- readMVar gsMVar
                                              sendTextData conn . WSServerMessage $ ServerMessageGameState gs
                                              clientMsg <- unwrapWSClientMessage <$> receiveData conn :: IO (ClientMessage b c n)
                                              BS.putStrLn $ encode clientMsg
                                              case clientMsg of
                                                ClientMessageFail -> putStrLn "failed to do action"
                                                ClientMessageAction action -> modifyMVar_ gsMVar (return . doTurn (rules config) action) -- TODO: broadcast

                backupApp :: Application
                backupApp _ respond = respond $ responseLBS status400 [] "Not a WebSocket request"

        publicH :: ServerT Raw m
        publicH = serveDirectoryWebApp path

server :: forall b c n. JSONGame b c n => Port -> FilePath -> IO (EndScreen b n)
server port path = do putStrLn $ "Port is: " <> show port
                      putStrLn . ("Public files are: " <>) . unwords =<< listDirectory path

                      initial <- either (error . show) id <$> runConfiguredT config initState :: IO (GameState b c n) -- TODO: error?
                      gs <- newMVar initial

                      let app = serve api $ hoistServer api (\ r -> runReaderT r (ServerState gs)) (handler path config)
                      run port app
                      return undefined -- TODO: undefined behaviour
  where config = def
