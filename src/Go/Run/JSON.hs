{-# LANGUAGE KindSignatures #-}

module Go.Run.JSON ( JSONGame
                   , ServerMessage (..)
                   , ClientMessage (..)
                   ) where

import Data.Aeson
import GHC.Generics
import GHC.TypeLits

import Go.Game.Game
import Go.Game.Player
import Go.Game.State

class (Game b c n, Generic b, Generic c, FromJSON b, FromJSON c, ToJSON b, ToJSON c) => JSONGame b c n

data ServerMessage b c n = ServerMessageGameState (GameState b c n)
                         | ServerMessagePlayer (Maybe (PlayerN n))
                         | ServerMessageFail String
  deriving (Eq, Generic, Ord, Read, Show)

instance JSONGame b c n => FromJSON (ServerMessage b c n) where
instance JSONGame b c n => ToJSON (ServerMessage b c n) where

data ClientMessage b c (n :: Nat) = ClientMessageAction (Action c)
                                  | ClientMessagePlayer (Maybe (PlayerN n))
                                  | ClientMessageFail String
  deriving (Eq, Generic, Ord, Read, Show)

instance JSONGame b c n => FromJSON (ClientMessage b c n) where
instance JSONGame b c n => ToJSON (ClientMessage b c n) where
