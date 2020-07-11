{-# LANGUAGE TypeOperators #-}

module Go.Server.API ( API'
                     , APIWrongWS
                     , EndpointHTML
                     , EndpointWS'
                     , EndpointPublic
                     ) where

import Servant.API
import Servant.HTML.Lucid (HTML)

import Go.Server.GameId
import Go.Server.Html

-- TODO: use servant-rawm-1.0.0.0 when it works with ghcjs
type API' rawm = Capture "gameId" GameId :> EndpointHTML
            :<|> EndpointHTML
            :<|> EndpointWS' rawm
            :<|> EndpointPublic

type APIWrongWS = API' Raw

type EndpointHTML = Get '[HTML] GameHtml
type EndpointWS' rawm = "ws" :> rawm
type EndpointPublic = "public" :> Raw
