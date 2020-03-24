module Go.Board.Loop ( BoardLoop (..)
                     , CoordXY (..)
                     , PlayerBW (..)
                     ) where

import Go.Board.Default
import Go.Frontend.Term
import Go.Game.Game

newtype BoardLoop = BLoop BoardSquare
  deriving Eq

instance Show BoardLoop where
  show (BLoop board) = show board

instance Board BoardLoop CoordXY where
  empty = BLoop (empty :: BoardSquare)
  coords (BLoop board) = coords board

  libertyCoords (BLoop (BSquare size vec)) (XY x y) = filter (flip elem $ coords (BLoop (BSquare size vec))) $ map wrap unsafeLibertyCoords
    where unsafeLibertyCoords = [ XY (x-1) y
                                , XY x     (y+1)
                                , XY (x+1) y
                                , XY x     (y-1)
                                ]
          wrap :: CoordXY -> CoordXY
          wrap (XY a b) = XY (a `mod` size) b

instance Game BoardLoop CoordXY PlayerBW where
  getStone (BLoop board) = getStone board
  putStone (BLoop board) coord stone = BLoop $ putStone board coord stone

instance TermGame BoardLoop CoordXY PlayerBW where
  readCoord (BLoop board) = readCoord board
