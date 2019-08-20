{-# LANGUAGE MultiParamTypeClasses #-}

module Board.Loop ( PlayerBW (..)
                  , CoordXY (..)
                  , BoardSquare (..)
                  , emptyFromSize
                  ) where

import Rules
import Board.Default

data BoardLoop = BLoop BoardSquare
  deriving (Eq, Show)

instance Board BoardLoop CoordXY where
  empty = BLoop (empty :: BoardSquare)
  coords (BLoop board) = coords board
  readCoordOnBoard (BLoop board) str = readCoordOnBoard board str

  libertyCoords (BLoop (BSquare size _)) (XY x y) = map wrap unsafeLibertyCoords
    where unsafeLibertyCoords = [ XY (x-1) y
                                , XY x     (y+1)
                                , XY (x+1) y
                                , XY x     (y-1)
                                ]
          wrap :: CoordXY -> CoordXY
          wrap (XY a b) = XY (a `mod` size) (b `mod` size)
