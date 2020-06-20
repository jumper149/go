module Game.Operation ( GameOperation (..)
                      ) where

import GHC.Generics

import qualified Go.Game.Player as G
import qualified Go.Game.State as G

data GameOperation b c n = UpdateAction (Maybe (G.Action c))
                         | SubmitAction
                         | SetState (G.GameState b c n)
                         | SubmitPlayer (Maybe (G.PlayerN n))
                         | SetPlayer (Maybe (G.PlayerN n))
  deriving (Eq, Ord, Generic, Read, Show)
