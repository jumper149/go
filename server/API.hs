{-# LANGUAGE FlexibleContexts, OverloadedStrings, TypeOperators #-}

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

type API = "game"   :> Capture "gameId" GameId :> Get '[HTML] GameHtml
      :<|> "ws"     :> Capture "gameId" GameId :> RawM
      :<|> "public" :> Raw

api :: Proxy API
api = Proxy

handler :: FilePath -> ServerT API (ServerStateT Handler)
handler path = gameH :<|> wssH :<|> publicH
  where gameH :: Monad m => GameId -> m GameHtml
        gameH _ = return GameHtml { jsAppPath = "../public/all.js" } -- TODO: Use path instead of hardcoded
        -- TODO: use argument to check if game exists

        wssH :: MonadBaseControl IO m => GameId -> ServerStateT m Application
        wssH gameId = liftTrans $ runMiddlewareT (websocketMiddleware gameId) <*> pure backupApp
          where backupApp :: Application
                backupApp _ respond = respond $ responseLBS status400 [] "Not a WebSocket request"

        publicH :: ServerT Raw m
        publicH = serveDirectoryWebApp path
