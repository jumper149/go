{-# LANGUAGE FunctionalDependencies #-}

module DefaultBoard ( Player (..)
                    , Stone (..)
                    , Coord (..)
                    , Board (..)
                    ) where

import qualified Board as B

import qualified Data.Vector as V

-- | Represents the players.
data Player = Black
            | White
  deriving (Eq, Enum, Bounded)

instance B.Player Player

-- | Represents the state of a point on the board.
data Stone = Free
           | Stone Player
           | Territory Player
  deriving Eq

instance Show Stone where
  show Free = "f"
  show (Stone Black) = "B"
  show (Stone White) = "W"
  show (Territory Black) = "b"
  show (Territory White) = "w"

instance B.Stone Stone Player where
  free = Free
  stone = Stone
  territory = Territory

-- | Represents the coordinates of a point on the board. Holds the x- and y-coordinate.
-- Coordinates are integers in the interval [0, boardsize).
data Coord = Coord Int Int
  deriving (Show , Eq)

instance B.Coord Coord

-- | Transform coordinate to index to access the array of points on the board.
coordToVecInd :: BoardSize -> Coord -> Int
coordToVecInd n (Coord x y)
  | max x y < n^2 = x + n * y
  | otherwise = undefined

-- Represents a square board. Contains the BoardSize and a Vector with all points.
data Board = Board BoardSize (V.Vector Stone)
  deriving Eq

instance Show Board where
  show (Board size vec) = ("\n" ++) $ concat $ map showRow rows
    where showRow = (++ "\n") . concat . V.map show
          rows = map slice [ i * size | i <- [0..(size-1)] ] :: [V.Vector Stone]
          slice n = V.slice n size vec

instance B.Board Board

-- | Represents the number of rows (or columns) on a square board.
type BoardSize = Int

-- | Defines the default board size.
defaultBoardSize = 19 :: BoardSize

-- | Create an empty board.
emptyFromSize :: BoardSize -> Board
emptyFromSize size = Board size (V.replicate (size^2) Free)

-- | Check if a coordinate is on the board.
coordOnBoard :: BoardSize -> Coord -> Bool
coordOnBoard size (Coord x y)
  | x < 0 = False
  | y < 0 = False
  | x > size = False
  | y > size = False
  | otherwise = True

-- | Return the neighboring coordinates on the board (next to or diagonally next to).
neighborCoords :: Board -> Coord -> [Coord]
neighborCoords (Board size _) (Coord x y) = filter (coordOnBoard size) unsafeNeighbors
  where unsafeNeighbors = [ Coord (x-1) (y-1)
                          , Coord (x-1) y
                          , Coord (x-1) (y+1)
                          , Coord x     (y+1)
                          , Coord (x+1) (y+1)
                          , Coord (x+1) y
                          , Coord (x+1) (y-1)
                          , Coord x     (y-1)
                          ]

-- | Return the stone on the given coordinate of the board.
getStone :: Board -> Coord -> Stone
getStone (Board size vec) coord = vec V.! coordToVecInd size coord

-- | Place a stone on a given coordinate of the board. Return the new board.
putStone :: Board -> Coord -> Stone -> Board
putStone (Board size vec) coord stone
  | oldStone == Free = Board size newVec
  | otherwise = undefined
  where oldStone = getStone (Board size vec) coord
        newVec = V.update vec $ V.singleton (coordToVecInd size coord , stone)

instance B.Gear Board Coord Stone Player where
  empty = emptyFromSize defaultBoardSize
  neighborCoords = neighborCoords
  getStone = getStone
  putStone = putStone

-- | Update the territory created by a newly placed stone. Return the new board.
--updateTerritory :: Coord -> Board -> Board
--updateTerritory coord (Board size vec)
