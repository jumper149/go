module Go.Board ( BoardRep (..)
                ) where

import GHC.Generics

import qualified Go.Board.Default as D

data BoardRep = BoardD_9_2 (D.Board 9 2)
              | BoardD_13_2 (D.Board 13 2)
  deriving Generic
