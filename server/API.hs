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

import Html
import ServerState
import WebSocket

import Go.Run.JSON

type API =             Get '[HTML] GameHtml
      :<|> "wss"    :> RawM
      :<|> "public" :> Raw

api :: Proxy API
api = Proxy

handler :: forall b c n. JSONGame b c n => FilePath -> ServerT API (ServerStateT b c n Handler)
handler path = gameH :<|> wssH :<|> publicH
  where gameH :: Monad m => m GameHtml
        gameH = return GameHtml { jsAppPath = "public/all.js" } -- TODO: Use path instead of hardcoded

        wssH :: MonadBaseControl IO m => ServerStateT b c n m Application
        wssH = liftTrans $ runMiddlewareT websocketMiddleware <*> pure backupApp
          where backupApp :: Application
                backupApp _ respond = respond $ responseLBS status400 [] "Not a WebSocket request"

        publicH :: ServerT Raw m
        publicH = serveDirectoryWebApp path
