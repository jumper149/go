{-# LANGUAGE TypeOperators #-}

module API ( API
           , EndpointPublic
           , api
           , handler
           ) where

import Control.Monad.Trans.Control
import Control.Monad.Trans.Control.Identity
import Network.HTTP.Types.Status (status400)
import Network.Wai (responseLBS)
import Network.Wai.Trans (runMiddlewareT)
import Servant
import Servant.HTML.Lucid (HTML)
import Servant.RawM (RawM)

import GameSet.Class
import Html
import ServerState
import WebSocket

type EndpointWSLobby = "ws" :> RawM
type EndpointPublic = "public" :> Raw

type API = "game"   :> Capture "gameId" GameId :> Get '[HTML] GameHtml
      :<|> "ws"     :> Capture "gameId" GameId :> RawM
      :<|> EndpointWSLobby
      :<|> EndpointPublic

api :: Proxy API
api = Proxy

handler :: FilePath -> ServerT API (ServerStateT Handler)
handler path = gameH :<|> wsGameH :<|> wsLobbyH :<|> publicH
  where gameH :: Monad m => GameId -> m GameHtml
        gameH _ = return GameHtml { jsAppPath = "/" <> urlPiece <> "/" <> appFile }
          where urlPiece = toUrlPiece $ safeLink api (Proxy :: Proxy EndpointPublic)
                appFile = "all.js"
          -- TODO: use argument to check if game exists

        wsGameH :: MonadBaseControl IO m => GameId -> ServerStateT m Application
        wsGameH gameId = liftTrans $ runMiddlewareT (websocketMiddleware $ Just gameId) <*> pure backupApp

        wsLobbyH :: MonadBaseControl IO m => ServerStateT m Application
        wsLobbyH = liftTrans $ runMiddlewareT (websocketMiddleware Nothing) <*> pure backupApp

        publicH :: ServerT Raw m
        publicH = serveDirectoryWebApp path

backupApp :: Application
backupApp _ respond = respond $ responseLBS status400 [] "Not a WebSocket request"
