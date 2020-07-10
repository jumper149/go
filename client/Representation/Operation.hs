{-# LANGUAGE UndecidableInstances #-}

module Representation.Operation ( GameOperationRep (..)
                                ) where

import GHC.Generics

import qualified Game.Board.Default as D
import Game.Operation

data GameOperationRep = GameOperationD_9_2 (GameOperation (D.Board 9 2))
                      | GameOperationD_13_2 (GameOperation (D.Board 13 2))
  deriving (Eq, Generic, Ord, Read, Show)
