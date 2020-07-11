{-# LANGUAGE TypeOperators #-}

module API ( API
           , api
           , handler
           ) where

import Control.Monad.Trans.Control
import Control.Monad.Trans.Control.Identity
import Network.HTTP.Types.Status (status400)
import Network.Wai (responseLBS)
import Network.Wai.Trans (runMiddlewareT)
import Servant.RawM (RawM)
import Servant

import Go.Server.API
import Go.Server.Html

import GameSet.Class
import ServerState
import WebSocket

type API = API' RawM

api :: Proxy API
api = Proxy

handler :: FilePath -> ServerT API (ServerStateT Handler)
handler path = const htmlH :<|> htmlH :<|> wsH :<|> publicH path

htmlH :: Monad m => m GameHtml
htmlH = return GameHtml { jsAppPath = "/" <> urlPiece <> "/" <> appFile }
  where urlPiece = toUrlPiece $ safeLink api (Proxy :: Proxy EndpointPublic)
        appFile = "all.js"
  -- TODO: use argument to check if game exists

wsH :: MonadBaseControl IO m => ServerStateT m Application
wsH = liftTrans $ runMiddlewareT websocketMiddleware <*> pure backupApp
  where backupApp _ respond = respond $ responseLBS status400 [] "Not a WebSocket request"

publicH :: FilePath -> ServerT Raw m
publicH = serveDirectoryWebApp
