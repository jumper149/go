module Action where

import GHC.Generics

import qualified Go.Board.Default as D
import qualified Go.Game.State as G

data Action = NoOp
            | QueueOp [Action]
            | UpdateAction (Maybe (G.Action D.Coord))
            | SubmitAction
  deriving (Eq, Ord, Generic, Read, Show)
