module Go.Run.JSON ( JSONGame
                   ) where

import Data.Aeson
import GHC.Generics

import Go.Game.Game

class (Game b c n, Generic b, Generic c, FromJSON b, FromJSON c, ToJSON b, ToJSON c) => JSONGame b c n
