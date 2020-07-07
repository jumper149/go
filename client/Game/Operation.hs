{-# LANGUAGE UndecidableInstances #-}

module Game.Operation ( GameOperation (..)
                      ) where

import GHC.Generics
import GHC.TypeLits

import qualified Go.Game.Game as G
import qualified Go.Game.State as G

data GameOperation b = UpdateAction (Maybe (G.AssociatedAction b))
                     | SubmitAction
                     | SetState (G.AssociatedGameState b)
                     | SubmitPlayer (Maybe (G.AssociatedPlayer b))
                     | SetPlayer (Maybe (G.AssociatedPlayer b))
  deriving Generic
deriving instance (Eq b, Eq (G.AssociatedCoord b)) => Eq (GameOperation b)
deriving instance (Ord b, Ord (G.AssociatedCoord b)) => Ord (GameOperation b)
deriving instance (KnownNat (G.AssociatedPlayerCount b), Read b, Read (G.AssociatedCoord b)) => Read (GameOperation b)
deriving instance (Show b, Show (G.AssociatedCoord b)) => Show (GameOperation b)
