{-# LANGUAGE TypeApplications #-}

module Message ( handleWS
               ) where

import Miso.WebSocket

import qualified Go.Message as G
import qualified Go.Representation as G
import qualified Go.Run.JSON as G

import Game.Operation
import Operation

handleWS :: forall b. (G.JSONGame b, G.RepresentableGame b) => WebSocket G.ServerMessageRep -> Operation b
handleWS (WebSocketMessage msg) = case msg of
                                    G.ServerMessageRepFail m -> WriteErrorLog (m <> "\n")
                                    G.ServerMessageGameStateRep gs -> maybe NoOp (GameOp . SetState) $ G.fromGameStateRep gs
                                    G.ServerMessagePlayerRep mbP -> maybe NoOp (GameOp . SetPlayer) $ traverse (G.fromPlayerRep @b) mbP -- TODO: why does this work, but type forcing the type with :: doesnt
handleWS _ = NoOp
