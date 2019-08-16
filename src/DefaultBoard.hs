{-# LANGUAGE MultiParamTypeClasses #-}

module DefaultBoard ( PlayerBW (..)
                    , Coord (..)
                    , BoardSquare (..)
                    , emptyFromSize
                    ) where

import Game
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
data Coord = Coord Int Int
  deriving (Eq, Ord, Show)

-- | Transform coordinate to index to access the array of points on the board.
coordToVecInd :: BoardSize -> Coord -> Int
coordToVecInd n (Coord x y) = x + n * y

-- | Represents a square board. Contains the BoardSize and a Vector with all points.
data BoardSquare = BSquare BoardSize (V.Vector (Stone PlayerBW))
  deriving Eq

-- | Represents the number of rows (or columns) on a square board.
type BoardSize = Int

-- | Defines the default board size.
defaultBoardSize = 4 :: BoardSize

-- | Create an empty board.
emptyFromSize :: BoardSize -> BoardSquare
emptyFromSize size = BSquare size (V.replicate (size^2) Free)

instance Show BoardSquare where
  show (BSquare size vec) = concatMap showRow rows
    where showRow = (++ "\n") . concat . V.map showStone
          rows = map slice [ i * size | i <- [0..(size-1)] ] :: [V.Vector (Stone PlayerBW)]
          slice n = V.slice n size vec

instance Board BoardSquare Coord where
  empty = emptyFromSize defaultBoardSize
  coords (BSquare size _) = [ Coord x y | x <- range , y <- range ]
    where range = [ 0 .. (size - 1) ]

  libertyCoords board (Coord x y) = filter (flip elem $ coords board) unsafeLibertyCoords
    where unsafeLibertyCoords = [ Coord (x-1) y
                                , Coord x     (y+1)
                                , Coord (x+1) y
                                , Coord x     (y-1)
                                ]

  readCoordOnBoard (BSquare size _) str = if length wrds == 2
                                        && charsInRange 48 57 x
                                        && charsInRange 97 122 y
                                        && length y == 1
                                        then Just (Coord xInt yInt)
                                        else Nothing
    where wrds = words str
          x = head wrds :: String
          y = head $ tail wrds :: String
          xInt = read x - 1 :: Int
          yInt = fromEnum (head y) - 97
          charsInRange :: Int -> Int -> String -> Bool
          charsInRange lo hi str = and bools
            where nums = map fromEnum str
                  bools = map (\ x -> x >= lo && x <= hi) nums

instance Game BoardSquare Coord PlayerBW where

  getStone (BSquare size vec) (Coord x y) = vec V.! coordToVecInd size (Coord x y)

  putStone (BSquare size vec) coord stone = BSquare size newVec
    where newVec = V.update vec $ V.singleton (coordToVecInd size coord , stone)

instance State BoardSquare Coord PlayerBW where

  display (BSquare size vec) player = numbers ++ bStr ++ pStr
    where bStr = unlines $ zipWith (:) alphabet $ (lines . show) (BSquare size vec)
          pStr = show player ++ "\n"
          numbers = " " ++ concatMap show [ 1 .. size ] ++ "\n"
          alphabet = map ((toEnum :: BoardSize -> Char) . (+ 96))  [ 1 .. size ]
