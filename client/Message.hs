module Message ( handleWS
               ) where

import Miso.WebSocket

import qualified Go.Run.JSON as G

import Game.Operation
import Operation

handleWS :: G.JSONGame b c n => WebSocket (G.ServerMessage b c n) -> Operation b c n
handleWS (WebSocketMessage msg) = case msg of
                                    G.ServerMessageFail m -> WriteErrorLog (m <> "\n")
                                    G.ServerMessageGameState gs -> GameOp $ SetState gs
                                    G.ServerMessagePlayer mbP -> GameOp $ SetPlayer mbP
handleWS _ = NoOp
