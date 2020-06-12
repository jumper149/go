module Operation ( Operation (..)
                 , handleWS
                 ) where

import GHC.Generics
import Miso.WebSocket

import qualified Go.Game.Player as G
import qualified Go.Game.State as G
import qualified Go.Run.JSON as G

data Operation b c n = NoOp
                     | QueueOp [Operation b c n]
                     | UpdateAction (Maybe (G.Action c))
                     | SubmitAction
                     | SetState (G.GameState b c n)
                     | SubmitPlayer (Maybe (G.PlayerN n))
                     | SetPlayer (Maybe (G.PlayerN n))
  deriving (Eq, Ord, Generic, Read, Show)

handleWS :: G.JSONGame b c n => WebSocket (G.ServerMessage b c n) -> Operation b c n
handleWS (WebSocketMessage msg) = case msg of
                                    G.ServerMessageFail -> NoOp
                                    G.ServerMessageGameState gs -> SetState gs
                                    G.ServerMessagePlayer mbP -> SetPlayer mbP
handleWS _ = NoOp
