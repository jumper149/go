module AwaitingGame.Html ( viewAwaitingGame
                         ) where

import Miso.Html
import Miso.String (ms)

import qualified Go.Run.GameId as G

import Operation

viewAwaitingGame :: G.GameId -> View Operation
viewAwaitingGame gameId = div_ [ class_ "awaiting-bar"
                               ] [ button_ [ onClick SubmitAwaitGame ] [ text $ "click here connect to game #" <> gameNo ]
                                 ]
                                     where gameNo = ms . show $ fromEnum gameId
