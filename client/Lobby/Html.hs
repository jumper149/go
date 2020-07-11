module Lobby.Html ( viewGames
                  ) where

import qualified Miso.Html as Html
import Miso.String (ms)

import qualified Go.GameId as G

viewGames :: [G.GameId] -> Html.View a
viewGames gs = Html.div_ [
                         ] $ fmap f gs
  where f g = Html.p_ [] [ Html.text $ ms $ show g ]
