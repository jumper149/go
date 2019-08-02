module Board ( createBoard
             , defaultBoardSize
             ) where

import qualified Data.Vector as V

-- | Represents the state of a point on the board.
data Stone = Free
           | White
           | Black 
  deriving Eq

instance Show Stone where
  show Free  = "f"
  show White = "W"
  show Black = "B"

-- | Represents the number of rows (or columns) on a square board.
type BoardSize = Int

-- | Defines the default board size.
defaultBoardSize = 19 :: BoardSize

-- Represents the board. Contains the BoardSize and a Vector representing all points.
data Board = Board BoardSize
                   (V.Vector Stone)
  deriving Eq

instance Show Board where
  show (Board size vec) = ("\n" ++) $ concat $ map showRow rows
    where showRow = (++ "\n") . concat . V.map show
          rows = map slice [ i * size | i <- [0..(size-1)] ] :: [V.Vector Stone]
          slice n = V.slice n size vec

-- | Create an empty board.
createBoard :: BoardSize -> Board
createBoard size = Board size
                         (V.replicate (size^2) Free)
