module Lobby.Operation ( LobbyOperation (..)
                       ) where

import GHC.Generics

import qualified Go.GameId as G

data LobbyOperation = UpdateGames [G.GameId]
  deriving (Eq, Ord, Generic, Read, Show)
