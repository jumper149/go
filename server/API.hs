{-# LANGUAGE TypeOperators #-}

module API ( API
           , api
           , handler
           ) where

import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Monad.Trans.Control.Identity
import GHC.Conc.Trans
import Network.HTTP.Types.Status (status400)
import Network.Wai (responseLBS)
import Network.Wai.Trans (runMiddlewareT)
import Servant.RawM (RawM)
import Servant

import Go.Run.API
import Go.Run.Html

import GameSet.Class
import ServerState
import ServerState.Class
import WebSocket

type API = API' RawM

api :: Proxy API
api = Proxy

handler :: FilePath -> ServerT API (ServerStateT Handler)
handler path = htmlGameH :<|> htmlH :<|> wsH :<|> publicH path

htmlGameH :: MonadBase IO m => GameId -> ServerStateT m GameHtml
htmlGameH gameId = do mbGame <- transact $ getGameSet gameId
                      case mbGame of
                        Nothing -> undefined -- TODO: implement a nice exception screen for AwaitingGame
                        Just _ -> htmlH

htmlH :: Monad m => m GameHtml
htmlH = return GameHtml { cssPath = publicPath <> "/" <> cssFile
                        , jsAppPath = publicPath <> "/" <> appFile
                        }
  where urlPiece = toUrlPiece $ safeLink api (Proxy :: Proxy EndpointPublic)
        publicPath = "/" <> urlPiece
        appFile = "all.js"
        cssFile = "stylesheet.css"

wsH :: MonadBaseControl IO m => ServerStateT m Application
wsH = liftTrans $ runMiddlewareT websocketMiddleware <*> pure backupApp
  where backupApp _ respond = respond $ responseLBS status400 [] "Not a WebSocket request"

publicH :: FilePath -> ServerT Raw m
publicH = serveDirectoryWebApp
