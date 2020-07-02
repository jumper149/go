{-# LANGUAGE FlexibleContexts, KindSignatures, StandaloneDeriving, UndecidableInstances #-}

module Go.Run.JSON ( JSONGame
                   , ServerMessage (..)
                   , ClientMessage (..)
                   ) where

import Data.Aeson
import GHC.Generics
import GHC.TypeLits

import Go.Game.Game
import Go.Game.State

class (Game b, Generic b, Generic (AssociatedCoord b), FromJSON b, FromJSON (AssociatedCoord b), ToJSON b, ToJSON (AssociatedCoord b)) => JSONGame b

data ServerMessage b = ServerMessageGameState (AssociatedGameState b)
                     | ServerMessagePlayer (Maybe (AssociatedPlayer b))
  deriving Generic
deriving instance (Eq b, Eq (AssociatedCoord b)) => Eq (ServerMessage b)
deriving instance (Ord b, Ord (AssociatedCoord b)) => Ord (ServerMessage b)
deriving instance (KnownNat (AssociatedPlayerCount b), Read b, Read (AssociatedCoord b)) => Read (ServerMessage b)
deriving instance (Show b, Show (AssociatedCoord b)) => Show (ServerMessage b)

instance JSONGame b => FromJSON (ServerMessage b) where
instance JSONGame b => ToJSON (ServerMessage b) where

data ClientMessage b = ClientMessageAction (AssociatedAction b)
                     | ClientMessagePlayer (Maybe (AssociatedPlayer b))
  deriving Generic
deriving instance (Eq b, Eq (AssociatedCoord b)) => Eq (ClientMessage b)
deriving instance (Ord b, Ord (AssociatedCoord b)) => Ord (ClientMessage b)
deriving instance (KnownNat (AssociatedPlayerCount b), Read b, Read (AssociatedCoord b)) => Read (ClientMessage b)
deriving instance (Show b, Show (AssociatedCoord b)) => Show (ClientMessage b)

instance JSONGame b => FromJSON (ClientMessage b) where
instance JSONGame b => ToJSON (ClientMessage b) where
