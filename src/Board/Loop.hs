module Board.Loop ( PlayerBW (..)
                  , CoordXY (..)
                  , BoardLoop (..)
                  ) where

import Rules
import Board.Default
import Frontend.Term.Term

data BoardLoop = BLoop BoardSquare
  deriving (Eq, Show)

instance Board BoardLoop CoordXY where
  empty = BLoop (empty :: BoardSquare)
  coords (BLoop board) = coords board
  readCoordOnBoard (BLoop board) str = readCoordOnBoard board str

  libertyCoords (BLoop (BSquare size vec)) (XY x y) = filter (flip elem $ coords (BLoop (BSquare size vec))) $ map wrap unsafeLibertyCoords
    where unsafeLibertyCoords = [ XY (x-1) y
                                , XY x     (y+1)
                                , XY (x+1) y
                                , XY x     (y-1)
                                ]
          wrap :: CoordXY -> CoordXY
          wrap (XY a b) = XY (a `mod` size) b

instance Game BoardLoop CoordXY PlayerBW where
  getStone (BLoop board) coord = getStone board coord
  putStone (BLoop board) coord stone = BLoop $ putStone board coord stone

instance TermGame BoardLoop CoordXY PlayerBW