module Go.Run.JSON ( JSONGame
                   ) where

import Data.Aeson
import GHC.Generics

import Go.Game.Game

class (Game b, Generic b, Generic (AssociatedCoord b), FromJSON b, FromJSON (AssociatedCoord b), ToJSON b, ToJSON (AssociatedCoord b)) => JSONGame b
