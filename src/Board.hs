module Board ( createBoard
             , defaultBoardSize
             , Stone (..)
             , Coord (..)
             , coordToVecInd
             , getStone
             , putStone
             ) where

import qualified Data.Vector as V

-- | Represents the players.
data Player = White
            | Black
  deriving Eq

-- | Represents the state of a point on the board.
data Stone = Free
           | Stone Player
           | Territory Player
  deriving Eq

instance Show Stone where
  show Free = "f"
  show (Stone White) = "W"
  show (Stone Black) = "B"
  show (Territory White) = "w"
  show (Territory Black) = "b"

-- | Represents the number of rows (or columns) on a square board.
type BoardSize = Int

-- | Defines the default board size.
defaultBoardSize = 19 :: BoardSize

-- Represents a square board. Contains the BoardSize and a Vector with all points.
data Board = Board BoardSize (V.Vector Stone)
  deriving Eq

instance Show Board where
  show (Board size vec) = ("\n" ++) $ concat $ map showRow rows
    where showRow = (++ "\n") . concat . V.map show
          rows = map slice [ i * size | i <- [0..(size-1)] ] :: [V.Vector Stone]
          slice n = V.slice n size vec

-- | Create an empty board.
createBoard :: BoardSize -> Board
createBoard size = Board size (V.replicate (size^2) Free)

-- | Represents the coordinates of a point on the board. Holds the x- and y-coordinate.
-- Coordinates are integers in the interval [0, boardsize).
data Coord = Coord Int Int
  deriving (Show , Eq)

-- | Transform coordinate to index to access the array of points on the board.
coordToVecInd :: BoardSize -> Coord -> Int
coordToVecInd n (Coord x y)
  | max x y < n^2 = x + n * y
  | otherwise = undefined

-- | Return the stone on the given coordinate of the board.
getStone :: Coord -> Board -> Stone
getStone coord (Board size vec) = vec V.! coordToVecInd size coord

-- | Place a stone on a given coordinate of the board. Return the new board.
putStone :: Coord -> Stone -> Board -> Board
putStone coord stone (Board size vec)
  | oldStone == Free = Board size newVec
  | otherwise = undefined
  where oldStone = getStone coord (Board size vec)
        newVec = V.update vec $ V.singleton (coordToVecInd size coord , stone)

-- | Check if a coordinate is on the board.
coordOnBoard :: BoardSize -> Coord -> Bool
coordOnBoard size (Coord x y)
  | x < 0 = False
  | y < 0 = False
  | x > size = False
  | y > size = False
  | otherwise = True

-- | Return the neighboring coordinates on the board (next to or diagonally next to).
neighborCoords :: BoardSize -> Coord -> [Coord]
neighborCoords size (Coord x y) = filter (coordOnBoard size) unsafeNeighbors
  where unsafeNeighbors = [ Coord (x-1) (y-1)
                          , Coord (x-1) y
                          , Coord (x-1) (y+1)
                          , Coord x     (y+1)
                          , Coord (x+1) (y+1)
                          , Coord (x+1) y
                          , Coord (x+1) (y-1)
                          , Coord x     (y-1)
                          ]

-- | Update the territory created by a newly placed stone. Return the new board.
--updateTerritory :: Coord -> Board -> Board
--updateTerritory coord (Board size vec)
