{-# LANGUAGE MultiParamTypeClasses #-}

module DefaultBoard ( Player (..)
                    , Coord (..)
                    , Board (..)
                    , emptyFromSize
                    ) where

import qualified Board as B

import qualified Data.Vector as V

-- | Represents the players.
data Player = Black
            | White
  deriving (Eq, Enum, Bounded, Ord, Show)

instance B.Player Player where
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
data Board = Board BoardSize (V.Vector (B.Stone Player))
  deriving Eq

-- | Represents the number of rows (or columns) on a square board.
type BoardSize = Int

-- | Defines the default board size.
defaultBoardSize = 4 :: BoardSize

-- | Create an empty board.
emptyFromSize :: BoardSize -> Board
emptyFromSize size = Board size (V.replicate (size^2) B.Free)

instance Show Board where
  show (Board size vec) = concatMap showRow rows
    where showRow = (++ "\n") . concat . V.map B.showStone
          rows = map slice [ i * size | i <- [0..(size-1)] ] :: [V.Vector (B.Stone Player)]
          slice n = V.slice n size vec

instance B.Board Board Coord where
  empty = emptyFromSize defaultBoardSize
  coords (Board size _) = [ Coord x y | x <- range , y <- range ]
    where range = [ 0 .. (size - 1) ]

  -- | Return the neighboring coordinates on the board (orthogonally next to).
  -- Invalid coordinates will be ignored by the class Board if the method coords is properly implemented.
  unsafeLibertyCoords (Board size _) (Coord x y) = [ Coord (x-1) y
                                                   , Coord x     (y+1)
                                                   , Coord (x+1) y
                                                   , Coord x     (y-1)
                                                   ]

  readCoordOnBoard (Board size _) str = if length wrds == 2
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

instance B.Game Board Coord Player where

  -- | Return the stone on the given coordinate of the board.
  getStone (Board size vec) (Coord x y) = vec V.! coordToVecInd size (Coord x y)

  -- | Place a stone on a given coordinate of the board. Return the new board.
  putStone (Board size vec) coord stone = Board size newVec
    where newVec = V.update vec $ V.singleton (coordToVecInd size coord , stone)

  -- | Turn board and player into an aesthetically good looking String.
  showGame (Board size vec) player = numbers ++ bStr ++ pStr
    where bStr = unlines $ zipWith (:) alphabet $ (lines . show) (Board size vec)
          pStr = show player ++ "\n"
          numbers = " " ++ concatMap show [ 1 .. size ] ++ "\n"
          alphabet = map ((toEnum :: BoardSize -> Char) . (+ 96))  [ 1 .. size ]
