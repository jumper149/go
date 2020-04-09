module Operation ( Operation (..)
                 ) where

import GHC.Generics

import qualified Go.Game.State as G

data Operation c = NoOp
                 | QueueOp [Operation c]
                 | UpdateAction (Maybe (G.Action c))
                 | SubmitAction
  deriving (Eq, Ord, Generic, Read, Show)
