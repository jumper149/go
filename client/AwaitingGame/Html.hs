module AwaitingGame.Html ( viewAwaitingGame
                         ) where

import Miso.Html
import Miso.String (ms)

import qualified Go.Run.GameId as G

import Operation

viewAwaitingGame :: G.GameId -> View Operation
viewAwaitingGame gameId = p_ [] [ text . ms $ "to connect to game #" <> show (fromEnum gameId) <> " click here: "
                                , button_ [ onClick SubmitAwaitGame ] [ text $ "Connect" ]
                                ]
