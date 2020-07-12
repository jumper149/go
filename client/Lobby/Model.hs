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
import qualified Go.Game.Rules as G
import qualified Go.Run.GameId as G
import qualified Go.Run.Message as G

import Lobby.Operation
import Lobby.Html

data LobbyModel = LobbyModel { availableGames :: [G.GameId]
                             , config :: G.Config
                             , submittable :: Bool
                             }
  deriving (Eq, Generic, Ord, Read, Show)

instance Default LobbyModel where
    def = LobbyModel { availableGames = mempty
                     , config = def
                     , submittable = True
                     }

updateLobbyModel :: LobbyOperation -> LobbyModel -> Effect LobbyOperation LobbyModel
updateLobbyModel operation model = case operation of
                                     LobbyNoOp -> noEff model
                                     UpdateGames gs -> noEff model { availableGames = gs }
                                     SubmitConfig -> model <# do send $ G.ClientMessageCreateGame $ config model
                                                                 return LobbyNoOp
                                     SetConfig sc -> let m = case sc of
                                                               SetConfigBoard b -> model { config = (config model) { G.board = b } }
                                                               SetConfigSize s -> model { config = (config model) { G.size = s } }
                                                               SetConfigPlayers p -> model { config = (config model) { G.players = p } }
                                                               SetConfigRules r -> case r of
                                                                                     SetConfigRulePassing p -> model { config = (config model) { G.ruleset = (G.ruleset $ config model) { G.passing = p } } }
                                                                                     SetConfigRuleKo k -> model { config = (config model) { G.ruleset = (G.ruleset $ config model) { G.ko = k } } }
                                                                                     SetConfigRuleSuicide s -> model { config = (config model) { G.ruleset = (G.ruleset $ config model) { G.suicide = s } } }
                                                     in updateLobbyModel (TryConfig $ config m) m
                                     TryConfig c -> model { submittable = False } <# do send $ G.ClientMessageTryConfig c
                                                                                        return LobbyNoOp
                                     ApproveConfig c -> noEff model { submittable = c == config model }

viewLobbyModel :: LobbyModel -> View LobbyOperation
viewLobbyModel LobbyModel { availableGames = gs, submittable = s } =
  div_ [
       ] [ viewCreateButton s
         , editConfig
         , viewGames gs
         ]
