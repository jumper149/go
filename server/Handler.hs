{-# LANGUAGE FlexibleContexts, OverloadedStrings, RecordWildCards #-}

module Handler where

import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Monad.Trans.Control.Identity
import Data.Foldable (traverse_)
import Network.HTTP.Types.Status (status400)
import Network.Wai (responseLBS)
import Network.Wai.Handler.WebSockets.Trans
import Network.Wai.Handler.WebSockets
import Network.Wai.Trans
import Network.WebSockets (defaultConnectionOptions)
import Servant

import API
import Html
import ServerState
import WebSocket

import Go.Run.JSON

handler :: forall b c n. JSONGame b c n => FilePath -> ServerT API (ServerStateT b c n Handler)
handler path = gameH :<|> wssH :<|> publicH
  where gameH :: Monad m => m GameHtml
        gameH = return GameHtml {..}
          where jsAppPath = "public/all.js" -- TODO: Use path instead of hardcoded

        wssH :: MonadBaseControl IO m => ServerStateT b c n m Application
        wssH = liftTrans $ runApplicationT $
                 websocketsOrT defaultConnectionOptions handleConnection $ liftApplication backupApp
          where backupApp :: Application
                backupApp _ respond = respond $ responseLBS status400 [] "Not a WebSocket request"

        publicH :: ServerT Raw m
        publicH = serveDirectoryWebApp path
