module Message ( handleWS
               ) where

import Miso.WebSocket

import qualified Go.Message as G

import Operation

handleWS :: WebSocket G.ServerMessageRep -> Operation
handleWS (WebSocketMessage msg) = case msg of
                                    G.ServerMessageRepFail m -> WriteErrorLog (m <> "\n")
                                    G.ServerMessageGameStateRep gs -> GameSetStateRep gs
                                    G.ServerMessagePlayerRep mbP -> GameSetPlayerRep mbP
                                    G.ServerMessageRepLobby gs -> LobbyOp $ UpdateGames gs
handleWS _ = NoOp
