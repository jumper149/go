{-# LANGUAGE UndecidableInstances #-}

module Operation ( Operation (..)
                 , LobbyOp (..)
                 ) where

import GHC.Generics

import qualified Go.Game as G
import qualified Go.Player as G

import Representation.Operation

data Operation = NoOp
               | QueueOp [Operation]
               | GameOp GameOperationRep
               | GameSetPlayerRep (Maybe G.PlayerRep)
               | GameSetStateRep G.GameStateRep
               | LobbyOp LobbyOp
               | WriteErrorLog String
  deriving (Eq, Generic, Ord, Read, Show)

data LobbyOp = AskAvailableGames
             | JoinGame String
  deriving (Eq, Ord, Generic, Read, Show)
