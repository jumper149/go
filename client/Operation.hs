{-# LANGUAGE UndecidableInstances #-}

module Operation ( Operation (..)
                 ) where

import GHC.Generics

import qualified Go.Game as G
import qualified Go.GameId as G
import qualified Go.Player as G

import Lobby.Operation
import Representation.Operation

data Operation = NoOp
               | QueueOp [Operation]
               | GameOp GameOperationRep
               | GameSetPlayerRep (Maybe G.PlayerRep)
               | GameSetStateRep G.GameStateRep
               | LobbyOp LobbyOperation
               | AwaitGame G.GameId
               | WriteErrorLog String
  deriving (Eq, Generic, Ord, Read, Show)
