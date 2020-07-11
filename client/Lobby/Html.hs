module Lobby.Html ( viewGames
                  ) where

import Data.Proxy
import Miso.Html
import Miso.String (ms)
import Servant.API

import qualified Go.Server.API as G
import qualified Go.Server.GameId as G

viewGames :: [G.GameId] -> View a
viewGames gs = div_ [] $ map viewGame gs

viewGame :: G.GameId -> View a
viewGame g = a_ [ href_ $ ms url ] [ text $ ms $ show g ]
  where url = ("/" <>) . toUrlPiece $ safeLink G.apiWrongWS (Proxy :: Proxy G.EndpointGame) g
