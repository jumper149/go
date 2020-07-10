{-# LANGUAGE UndecidableInstances #-}

module Representation.Model ( GameModelRep (..)
                            ) where

import GHC.Generics

import qualified Game.Board.Default as D
import Game.Model

data GameModelRep = GameModelD_9_2 (GameModel (D.Board 9 2))
                  | GameModelD_13_2 (GameModel (D.Board 13 2))
  deriving (Eq, Generic, Ord, Read, Show)
