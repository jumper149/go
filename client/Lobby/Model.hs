module Lobby.Model ( LobbyModel (..)
                   , updateLobbyModel
                   , viewLobbyModel
                   ) where

import Data.Default.Class
import GHC.Generics
import Miso.Effect
import Miso.Html
import Miso.Subscription.WebSocket

import qualified Go.Config as G
import qualified Go.GameId as G
import qualified Go.Message as G

import Lobby.Operation
import Lobby.Svg
import Lobby.Html

data LobbyModel = LobbyModel { availableGames :: [G.GameId]
                             , config :: G.Config
                             }
  deriving (Eq, Generic, Ord, Read, Show)

instance Default LobbyModel where
    def = LobbyModel { availableGames = mempty
                     , config = def
                     }

updateLobbyModel :: LobbyOperation -> LobbyModel -> Effect LobbyOperation LobbyModel
updateLobbyModel operation model = case operation of
                                     LobbyNoOp -> noEff model
                                     UpdateGames gs -> noEff model { availableGames = gs }
                                     SubmitConfig -> model <# do send $ G.ClientMessageRepCreateGame $ config model
                                                                 return LobbyNoOp

viewLobbyModel :: LobbyModel -> View LobbyOperation
viewLobbyModel LobbyModel { availableGames = gs } =
  div_ [
       ] [ viewCreateButton
         , viewGames gs
         ]
