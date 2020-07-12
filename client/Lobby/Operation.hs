module Lobby.Operation ( LobbyOperation (..)
                       , SetConfig (..)
                       , SetConfigRules (..)
                       ) where

import GHC.Generics

import qualified Go.Config as G
import qualified Go.Game.Rules as G
import qualified Go.Server.GameId as G

data LobbyOperation = LobbyNoOp
                    | UpdateGames [G.GameId]
                    | SubmitConfig
                    | SetConfig SetConfig
                    | TryConfig G.Config
                    | ApproveConfig G.Config
  deriving (Eq, Ord, Generic, Read, Show)

data SetConfig = SetConfigBoard G.BoardName
               | SetConfigSize G.BoardSize
               | SetConfigPlayers G.PlayerCount
               | SetConfigRules SetConfigRules
  deriving (Eq, Ord, Generic, Read, Show)

data SetConfigRules = SetConfigRulePassing G.Permission
                    | SetConfigRuleKo G.KoRule
                    -- | SetConfigRulesKomi G.Komi
                    | SetConfigRuleSuicide G.Permission
  deriving (Eq, Ord, Generic, Read, Show)
