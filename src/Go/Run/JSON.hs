{-# LANGUAGE FlexibleContexts, KindSignatures, StandaloneDeriving, UndecidableInstances #-}

module Go.Run.JSON ( JSONGame
                   ) where

import Data.Aeson
import GHC.Generics
import GHC.TypeLits

import Go.Game.Game
import Go.Game.State

class (Game b, Generic b, Generic (AssociatedCoord b), FromJSON b, FromJSON (AssociatedCoord b), ToJSON b, ToJSON (AssociatedCoord b)) => JSONGame b
