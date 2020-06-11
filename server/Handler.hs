{-# LANGUAGE FlexibleContexts, KindSignatures, OverloadedStrings, RecordWildCards #-}

module Handler where

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
import Network.Wai.Handler.Warp (Port, run)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets
import Servant
import System.Directory (listDirectory)

import API
import Html
import Message

import Go.Game.Config
import Go.Game.Playing
import Go.Game.State
import Go.Run.JSON

data ServerState b c n = ServerState { gameStateMVar :: MVar (GameState b c n)
                                     , gameConfig :: Config
                                     }

newtype HandlerM b c n a = HandlerM { unwrapHandlerM :: ReaderT (ServerState b c n) Handler a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader (ServerState b c n))

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
                  return $ websocketsOr defaultConnectionOptions (handleWSConnection gs gc) backupApp
          where backupApp :: Application
                backupApp _ respond = respond $ responseLBS status400 [] "Not a WebSocket request"

        publicH :: ServerT Raw m
        publicH = serveDirectoryWebApp path

handleWSConnection :: forall b c n. JSONGame b c n => MVar (GameState b c n) -> Config -> PendingConnection -> IO ()
handleWSConnection gsMVar config pendingConn = do conn <- acceptRequest pendingConn
                                                  gs <- readMVar gsMVar
                                                  sendTextData conn . WSServerMessage $ ServerMessageGameState gs
                                                  clientMsg <- unwrapWSClientMessage <$> receiveData conn :: IO (ClientMessage b c n)
                                                  BS.putStrLn $ encode clientMsg
                                                  case clientMsg of
                                                    ClientMessageFail -> putStrLn "failed to do action"
                                                    ClientMessageAction action -> modifyMVar_ gsMVar (return . doTurn (rules config) action) -- TODO: broadcast
