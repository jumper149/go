{-# LANGUAGE FlexibleContexts, KindSignatures, StandaloneDeriving, UndecidableInstances #-}

module Go.Run.JSON ( JSONGame
                   , AssociatedServerMessage (..)
                   , AssociatedClientMessage (..)
                   ) where

import Data.Aeson
import GHC.Generics
import GHC.TypeLits

import Go.Game.Game
import Go.Game.State

class (Game b, Generic b, Generic (AssociatedCoord b), FromJSON b, FromJSON (AssociatedCoord b), ToJSON b, ToJSON (AssociatedCoord b)) => JSONGame b

-- TODO: remove all messages from this module?
data AssociatedServerMessage b = ServerMessageGameState (AssociatedGameState b)
                               | ServerMessagePlayer (Maybe (AssociatedPlayer b))
  deriving Generic
deriving instance (Eq b, Eq (AssociatedCoord b)) => Eq (AssociatedServerMessage b)
deriving instance (Ord b, Ord (AssociatedCoord b)) => Ord (AssociatedServerMessage b)
deriving instance (KnownNat (AssociatedPlayerCount b), Read b, Read (AssociatedCoord b)) => Read (AssociatedServerMessage b)
deriving instance (Show b, Show (AssociatedCoord b)) => Show (AssociatedServerMessage b)

instance JSONGame b => FromJSON (AssociatedServerMessage b) where
instance JSONGame b => ToJSON (AssociatedServerMessage b) where

data AssociatedClientMessage b = ClientMessageAction (AssociatedAction b)
                               | ClientMessagePlayer (Maybe (AssociatedPlayer b))
  deriving Generic
deriving instance (Eq b, Eq (AssociatedCoord b)) => Eq (AssociatedClientMessage b)
deriving instance (Ord b, Ord (AssociatedCoord b)) => Ord (AssociatedClientMessage b)
deriving instance (KnownNat (AssociatedPlayerCount b), Read b, Read (AssociatedCoord b)) => Read (AssociatedClientMessage b)
deriving instance (Show b, Show (AssociatedCoord b)) => Show (AssociatedClientMessage b)

instance JSONGame b => FromJSON (AssociatedClientMessage b) where
instance JSONGame b => ToJSON (AssociatedClientMessage b) where
