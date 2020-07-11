module Lobby.Operation ( LobbyOperation (..)
                       ) where

import GHC.Generics

import qualified Go.Server.GameId as G

data LobbyOperation = LobbyNoOp
                    | UpdateGames [G.GameId]
                    | SubmitConfig
  deriving (Eq, Ord, Generic, Read, Show)
