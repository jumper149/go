{-# LANGUAGE MultiParamTypeClasses #-}

module DefaultBoard ( PlayerBW (..)
                    , CoordXY (..)
                    , BoardSquare (..)
                    , emptyFromSize
                    ) where

import Rules
import GameState

import qualified Data.Vector as V

-- | Represents the players.
data PlayerBW = Black
              | White
  deriving (Eq, Enum, Bounded, Ord, Show)

instance Player PlayerBW where
  char Black = 'B'
  char White = 'W'

-- | Represents the coordinates of a point on the board. Holds the x- and y-coordinate.
-- Coordinates are integers in the interval [0, boardsize).
data CoordXY = XY Int Int
  deriving (Eq, Ord, Show)

-- | Transform coordinate to index to access the array of points on the board.
coordToVecInd :: BoardSize -> CoordXY -> Int
coordToVecInd n (XY x y) = x + n * y

-- | Represents a square board. Contains the BoardSize and a Vector with all points.
data BoardSquare = BSquare BoardSize (V.Vector (Stone PlayerBW))
  deriving Eq

-- | Represents the number of rows (or columns) on a square board.
type BoardSize = Int

-- | Defines the default board size.
defaultBoardSize :: BoardSize
defaultBoardSize = 4

-- | Create an empty board.
emptyFromSize :: BoardSize -> BoardSquare
emptyFromSize size = BSquare size (V.replicate (size * size) Free)

instance Show BoardSquare where
  show (BSquare size vec) = concatMap showRow rows
    where showRow = (++ "\n") . concat . V.map showStone
          rows = map slice [ i * size | i <- [0..(size-1)] ] :: [V.Vector (Stone PlayerBW)]
          slice n = V.slice n size vec

instance Board BoardSquare CoordXY where
  empty = emptyFromSize defaultBoardSize
  coords (BSquare size _) = [ XY x y | x <- range , y <- range ]
    where range = [ 0 .. (size - 1) ]

  libertyCoords board (XY x y) = filter (flip elem $ coords board) unsafeLibertyCoords
    where unsafeLibertyCoords = [ XY (x-1) y
                                , XY x     (y+1)
                                , XY (x+1) y
                                , XY x     (y-1)
                                ]

  readCoordOnBoard board str = if length wrds == 2
                               && charsInRange 48 57 x
                               && charsInRange 97 122 y
                               && length y == 1
                               then let coord = XY xInt yInt
                                    in if coord `elem` coords board
                                       then Just coord
                                       else Nothing
                               else Nothing
    where wrds = words str
          x = head wrds :: String
          y = head $ tail wrds :: String
          xInt = read x - 1 :: Int
          yInt = fromEnum (head y) - 97
          charsInRange :: Int -> Int -> String -> Bool
          charsInRange lo hi st = and bools
            where nums = map fromEnum st
                  bools = map (\ i -> i >= lo && i <= hi) nums

instance Game BoardSquare CoordXY PlayerBW where

  getStone (BSquare size vec) (XY x y) = vec V.! coordToVecInd size (XY x y)

  putStone (BSquare size vec) coord stone = BSquare size newVec
    where newVec = V.update vec $ V.singleton (coordToVecInd size coord , stone)

instance StateTerm BoardSquare CoordXY PlayerBW where

  display (BSquare size vec) player = numbers ++ bStr ++ pStr
    where bStr = unlines $ zipWith (:) alphabet $ (lines . show) (BSquare size vec)
          numbers = " " ++ concatMap show [ 1 .. size ] ++ "\n"
          alphabet = map ((toEnum :: BoardSize -> Char) . (+ 96))  [ 1 .. size ]
          pStr = show player ++ "\n"
