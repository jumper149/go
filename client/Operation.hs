{-# LANGUAGE UndecidableInstances #-}

module Operation ( Operation (..)
                 ) where

import GHC.Generics

import qualified Go.Representation as G
import qualified Go.Run.GameId as G

import Lobby.Operation
import Representation.Operation

data Operation = NoOp
               | QueueOp [Operation]
               | GameOp GameOperationRep
               | GameSetPlayerRep (Maybe G.PlayerRep)
               | GameSetStateRep G.GameStateRep
               | LobbyOp LobbyOperation
               | SetAwaitGame G.GameId
               | SubmitAwaitGame
               | WriteErrorLog String
  deriving (Eq, Generic, Ord, Read, Show)
