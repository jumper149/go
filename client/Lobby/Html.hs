module Lobby.Html ( viewGames
                  ) where

import Miso.Html
import Miso.String (ms)

import qualified Go.Server.GameId as G

viewGames :: [G.GameId] -> View a
viewGames gs = div_ [] $ map viewGame gs

viewGame :: G.GameId -> View a
viewGame g = a_ [] [ text $ ms $ show g ]
