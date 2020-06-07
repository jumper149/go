module Operation ( Operation (..)
                 , handleWS
                 ) where

import GHC.Generics
import Miso.WebSocket

import qualified Go.Game.State as G
import qualified Go.Run.JSON as G

data Operation b c n = NoOp
                     | QueueOp [Operation b c n]
                     | UpdateAction (Maybe (G.Action c))
                     | SubmitAction
                     | SetState (G.GameState b c n)
  deriving (Eq, Ord, Generic, Read, Show)

handleWS :: G.JSONGame b c n => WebSocket (G.ServerMessage b c n) -> Operation b c n
handleWS (WebSocketMessage msg) = case msg of
                                    G.ServerMessageFail -> NoOp
                                    G.ServerMessageGameState gs -> SetState gs
handleWS _ = NoOp
