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
import Servant
import Servant.HTML.Lucid (HTML)
import Servant.RawM (RawM)

import GameSet.Class
import Html
import ServerState
import WebSocket


type API = Capture "gameId" GameId :> GameAPI
      :<|> EndpointHTML
      :<|> EndpointWS
      :<|> EndpointPublic

type GameAPI = EndpointHTML
          :<|> EndpointWS

type EndpointHTML = Get '[HTML] GameHtml
type EndpointWS = "ws" :> RawM
type EndpointPublic = "public" :> Raw

api :: Proxy API
api = Proxy

handler :: FilePath -> ServerT API (ServerStateT Handler)
handler path = handlerGame :<|> htmlH :<|> wsH Nothing :<|> publicH path
  where publicH :: FilePath -> ServerT Raw m
        publicH = serveDirectoryWebApp

handlerGame :: GameId -> ServerT GameAPI (ServerStateT Handler)
handlerGame gameId = htmlH :<|> wsH (Just gameId)

wsH :: MonadBaseControl IO m => Maybe GameId -> ServerStateT m Application
wsH mbGameId = liftTrans $ runMiddlewareT (websocketMiddleware mbGameId) <*> pure backupApp
  where backupApp _ respond = respond $ responseLBS status400 [] "Not a WebSocket request"

htmlH :: Monad m => m GameHtml
htmlH = return GameHtml { jsAppPath = "/" <> urlPiece <> "/" <> appFile }
  where urlPiece = toUrlPiece $ safeLink api (Proxy :: Proxy EndpointPublic)
        appFile = "all.js"
  -- TODO: use argument to check if game exists
