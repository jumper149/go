{-# LANGUAGE TypeOperators #-}

module Go.Run.API ( API'
                  , EndpointGame
                  , EndpointHTML
                  , EndpointWS'
                  , EndpointPublic
                  , APIWrongWS
                  , apiWrongWS
                  , EndpointWSWrongWS
                  ) where

import Data.Proxy
import Servant.API
import Servant.HTML.Lucid (HTML)

import Go.Run.GameId
import Go.Run.Html

-- TODO: use servant-rawm-1.0.0.0 when it works with ghcjs
type API' rawm = EndpointGame
            :<|> EndpointHTML
            :<|> EndpointWS' rawm
            :<|> EndpointPublic

type APIWrongWS = API' Raw
type EndpointWSWrongWS = EndpointWS' Raw

apiWrongWS :: Proxy APIWrongWS
apiWrongWS = Proxy

type EndpointGame = Capture "gameId" GameId :> EndpointHTML
type EndpointHTML = Get '[HTML] GameHtml
type EndpointWS' rawm = "ws" :> rawm
type EndpointPublic = "public" :> Raw
