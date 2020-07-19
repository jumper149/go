{-# LANGUAGE TypeOperators #-}

module Go.Run.API ( API
                  , api
                  , EndpointGame
                  , EndpointHTML
                  , EndpointWS
                  , EndpointPublic
                  ) where

import Data.Proxy
import Servant.API
import Servant.HTML.Lucid (HTML)
import Servant.RawM (RawM)

import Go.Run.GameId
import Go.Run.Html

type API  = EndpointGame
       :<|> EndpointHTML
       :<|> EndpointWS
       :<|> EndpointPublic

api :: Proxy API
api = Proxy

type EndpointGame = Capture "gameId" GameId :> EndpointHTML
type EndpointHTML = Get '[HTML] GameHtml
type EndpointWS = "ws" :> RawM
type EndpointPublic = "public" :> Raw
