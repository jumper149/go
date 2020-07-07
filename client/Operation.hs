{-# LANGUAGE UndecidableInstances #-}

module Operation ( Operation (..)
                 , LobbyOp (..)
                 ) where

import GHC.Generics
import GHC.TypeLits

import qualified Go.Game.Game as G

import Game.Operation

data Operation b = NoOp
                 | QueueOp [Operation b]
                 | GameOp (GameOperation b)
                 | LobbyOp LobbyOp
                 | WriteErrorLog String
  deriving Generic
deriving instance (Eq b, Eq (G.AssociatedCoord b)) => Eq (Operation b)
deriving instance (Ord b, Ord (G.AssociatedCoord b)) => Ord (Operation b)
deriving instance (KnownNat (G.AssociatedPlayerCount b), Read b, Read (G.AssociatedCoord b)) => Read (Operation b)
deriving instance (Show b, Show (G.AssociatedCoord b)) => Show (Operation b)

data LobbyOp = AskAvailableGames
             | JoinGame String
  deriving (Eq, Ord, Generic, Read, Show)
