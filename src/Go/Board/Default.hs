module Go.Board.Default ( BoardSquare (..)
                        , BoardSize
                        , CoordXY (..)
                        , PlayerBW (..)
                        ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import GHC.Generics (Generic)

import Go.Game.Config
import Go.Game.Game
import Go.Run.Server.JSON
import Go.Run.Term

-- | Represents the players.
data PlayerBW = Black
              | White
  deriving (Eq, Enum, Bounded, Ord, Show, Generic)

instance Player PlayerBW
instance FromJSON PlayerBW
instance ToJSON PlayerBW

-- | Represents the coordinates of a point on the board. Holds the x- and y-coordinate.
-- Coordinates are integers in the interval [0, boardsize).
data CoordXY = XY Int Int
  deriving (Eq, Ord, Show, Generic)

instance FromJSON CoordXY
instance ToJSON CoordXY

-- | Transform coordinate to index to access the array of points on the board.
coordToVecInd :: BoardSize -> CoordXY -> Int
coordToVecInd s (XY x y) = x + getBoardSize s * y

-- | Represents a square board. Contains the 'BoardSize' and a 'V.Vector' with all points.
data BoardSquare = BSquare BoardSize (V.Vector (Stone PlayerBW))
  deriving (Eq, Generic)

instance FromJSON BoardSquare
instance ToJSON BoardSquare

-- | Represents the number of rows (or columns) on a square board.
newtype BoardSize = BoardSize { getBoardSize :: Int }
  deriving (Eq, Ord, Generic)

instance FromJSON BoardSize
instance ToJSON BoardSize

instance Bounded BoardSize where
  minBound = BoardSize { getBoardSize = 0 }
  maxBound = BoardSize { getBoardSize = 26 }

instance Enum BoardSize where
  toEnum s = fromMaybe undefined $ boardSize s
  fromEnum s = getBoardSize s

-- | Create 'BoardSize'.
boardSize :: Int -> Maybe BoardSize
boardSize s
  | s > 0 && s <= 26 = Just $ BoardSize { getBoardSize = s }
  | otherwise = Nothing

instance Show BoardSquare where
  show (BSquare s vec) = decNumbers ++ numbers ++ bStr
    where bStr = unlines $ zipWith (:) alphabet $ (lines . showRaw) (BSquare s vec)
          numbers = " " ++ concatMap (show . (`mod` 10)) [ 1 .. fromEnum s ] ++ "\n"
          decNumbers = if fromEnum s >= 10 then decNumbersRaw else ""
          decNumbersRaw = " " ++ concatMap ((++ "         ") . show) [ 0 .. fromEnum s `div` 10 ] ++ "\n"
          alphabet = map ((toEnum :: Int -> Char) . (+ 96))  [ 1 .. fromEnum s ]

showRaw :: BoardSquare -> String
showRaw (BSquare s vec) = concatMap showRow rows
  where showRow = (++ "\n") . concat . V.map showStone
        rows = map slice [ i * fromEnum s | i <- [0..(fromEnum s - 1)] ] :: [V.Vector (Stone PlayerBW)]
        slice n = V.slice n (fromEnum s) vec

-- | Show a stone as a single character string.
showStone :: Stone PlayerBW -> String
showStone Free = " "
showStone (Stone p) = [ char p ]

char :: PlayerBW -> Char
char Black = 'B'
char White = 'W'

instance Board BoardSquare CoordXY where
  empty config = do s <- boardSize $ size config
                    return . BSquare s $ V.replicate (fromEnum s * fromEnum s) Free

  coords (BSquare s _) = [ XY x y | x <- range , y <- range ]
    where range = [ 0 .. (fromEnum s - 1) ]

  libertyCoords board (XY x y) = filter (flip elem $ coords board) unsafeLibertyCoords
    where unsafeLibertyCoords = [ XY (x-1) y
                                , XY x     (y+1)
                                , XY (x+1) y
                                , XY x     (y-1)
                                ]

instance Game BoardSquare CoordXY PlayerBW where
  getStone (BSquare s vec) (XY x y) = vec V.! coordToVecInd s (XY x y)

  putStone (BSquare s vec) coord stone = BSquare s newVec
    where newVec = V.update vec $ V.singleton (coordToVecInd s coord , stone)

instance TermGame BoardSquare CoordXY PlayerBW where
  readCoord board str = if length wrds == 2
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

instance JSONGame BoardSquare CoordXY PlayerBW
