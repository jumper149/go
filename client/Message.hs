module Message ( handleWS
               ) where

import Miso.WebSocket

import qualified Go.Run.Message as G

import Lobby.Operation
import Operation

handleWS :: WebSocket G.ServerMessage -> Operation
handleWS (WebSocketMessage msg) = case msg of
                                    G.ServerMessageFail m -> WriteErrorLog (m <> "\n")
                                    G.ServerMessageGameStateRep gs -> GameSetStateRep gs
                                    G.ServerMessagePlayerRep mbP -> GameSetPlayerRep mbP
                                    G.ServerMessageLobby gs -> LobbyOp $ UpdateGames gs
                                    G.ServerMessageApproveConfig c -> LobbyOp $ ApproveConfig c
handleWS _ = NoOp
