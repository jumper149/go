module Operation ( Operation (..)
                 ) where

import GHC.Generics

import qualified Go.Game.State as G

data Operation b c n = NoOp
                     | QueueOp [Operation b c n]
                     | UpdateAction (Maybe (G.Action c))
                     | SubmitAction
                     | SetState (G.GameState b c n)
  deriving (Eq, Ord, Generic, Read, Show)
