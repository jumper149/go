{-# LANGUAGE MultiParamTypeClasses #-}

module DefaultBoard ( Player (..)
                    , Coord (..)
                    , Board (..)
                    ) where

import qualified Board as B

import qualified Data.Vector as V

-- | Represents the players.
data Player = Black
            | White
  deriving (Eq, Enum, Bounded)

instance B.Player Player where
  char Black = 'B'
  char White = 'W'

-- | Represents the coordinates of a point on the board. Holds the x- and y-coordinate.
-- Coordinates are integers in the interval [0, boardsize).
-- Borders are also represented as Coordinates to cover edge cases.
data Coord = Coord Int Int
           | Border Border
  deriving (Eq, Show)

data Border = LeftB
            | TopB
            | RightB
            | BottomB
            | BottomLeftC
            | TopLeftC
            | TopRightC
            | BottomRightC
  deriving (Eq, Show)

-- | Transform coordinate to index to access the array of points on the board.
coordToVecInd :: BoardSize -> Coord -> Int
coordToVecInd n (Coord x y) = x + n * y
coordToVecInd _ (Border _) = -1

-- | Represents a square board. Contains the BoardSize and a Vector with all points.
data Board = Board BoardSize (V.Vector (B.Stone Player))
  deriving Eq

instance Show Board where
  show (Board size vec) = ("\n" ++) $ concatMap showRow rows
    where showRow = (++ "\n") . concat . V.map show
          rows = map slice [ i * size | i <- [0..(size-1)] ] :: [V.Vector (B.Stone Player)]
          slice n = V.slice n size vec

instance B.Board Board Coord where
  empty = emptyFromSize defaultBoardSize
  neighborCoords = neighborCoords
  libertyCoords = orthogonalNeighborCoords

-- | Represents the number of rows (or columns) on a square board.
type BoardSize = Int

-- | Defines the default board size.
defaultBoardSize = 19 :: BoardSize

-- | Create an empty board.
emptyFromSize :: BoardSize -> Board
emptyFromSize size = Board size (V.replicate (size^2) B.Free)

-- | Check if a coordinate is on the board and use Border/Corner constructors if necessary.
fixCoord :: BoardSize -> Coord -> Coord
fixCoord size (Coord x y)
  | x == -1 && y == -1 = Border BottomLeftC
  | x == -1 && y == size = Border TopLeftC
  | x == size && y == size = Border TopLeftC
  | x == size && y == -1 = Border BottomRightC
  | x == -1 = Border LeftB
  | y == size = Border TopB
  | x == size = Border RightB
  | y == -1 = Border BottomB
  | x >= 0 && x < size && y >= 0 && y < size = Coord x y
  | otherwise = undefined

-- | Return the neighboring coordinates on the board (next to or diagonally next to).
neighborCoords :: Board -> Coord -> [Coord]
neighborCoords (Board size _) (Coord x y) = map (fixCoord size) unsafeNeighbors
  where unsafeNeighbors = [ Coord (x-1) (y-1)
                          , Coord (x-1) y
                          , Coord (x-1) (y+1)
                          , Coord x     (y+1)
                          , Coord (x+1) (y+1)
                          , Coord (x+1) y
                          , Coord (x+1) (y-1)
                          , Coord x     (y-1)
                          ]

-- | Return the neighboring coordinates on the board (orthogonally next to).
orthogonalNeighborCoords :: Board -> Coord -> [Coord]
orthogonalNeighborCoords (Board size _) (Coord x y) = map (fixCoord size) unsafeNeighbors
  where unsafeNeighbors = [ Coord (x-1) y
                          , Coord x     (y+1)
                          , Coord (x+1) y
                          , Coord x     (y-1)
                          ]

-- | Return the stone on the given coordinate of the board.
getStone :: Board -> Coord -> B.Stone Player
getStone (Board size vec) (Coord x y) = vec V.! coordToVecInd size (Coord x y)
getStone (Board size vec) (Border _) = B.Off

-- | Place a stone on a given coordinate of the board. Return the new board.
putStone :: Board -> Coord -> B.Stone Player -> Board
putStone (Board size vec) coord stone
  | oldStone == B.Free = Board size newVec
  | otherwise = undefined
  where oldStone = getStone (Board size vec) coord
        newVec = V.update vec $ V.singleton (coordToVecInd size coord , stone)

instance B.Gear Board Coord Player where
  getStone = getStone
  putStone = putStone

-- | Update the territory created by a newly placed stone. Return the new board.
--updateTerritory :: Coord -> Board -> Board
--updateTerritory coord (Board size vec)
