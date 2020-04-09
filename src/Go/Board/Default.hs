module Go.Board.Default ( BoardSquare (..)
                        , BoardSize
                        , Coord (..)
                        , mkCoord
                        ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import GHC.TypeLits

import Go.Config
import Go.Game.Game
import Go.Game.Player
import Go.Run.JSON
import Go.Run.Term

-- | Represents the coordinates of a point on the board. Holds the x- and y-coordinate.
-- Coordinates are integers in the interval [0, boardsize).
data Coord = Coord { getX :: Int
                   , getY :: Int
                   }
  deriving (Bounded, Eq, Generic, Ord, Read, Show) -- TODO: nice Bounded instance, maybe Enum too?

instance FromJSON Coord
instance ToJSON Coord

-- | Smart constructor where coordinates are required to be in the interval [1..boardsize].
mkCoord :: BoardSize -> Int -> Int -> Maybe Coord
mkCoord s x y = if check x && check y
                   then Just $ Coord (x-1) (y-1)
                   else Nothing
  where check c = c <= getBoardSize s && c >= 1

-- | Transform coordinate to index to access the array of points on the board.
coordToVecInd :: BoardSize -> Coord -> Int
coordToVecInd s c = getX c + getBoardSize s * getY c

-- | Represents a square board. Contains the 'BoardSize' and a 'V.Vector' with all points.
data BoardSquare n = BSquare BoardSize (V.Vector (Stone (PlayerN n)))
  deriving (Eq, Generic, Ord, Read, Show)

instance KnownNat n => FromJSON (BoardSquare n)
instance KnownNat n => ToJSON (BoardSquare n)

-- | Represents the number of rows (or columns) on a square board.
newtype BoardSize = BoardSize { getBoardSize :: Int }
  deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON BoardSize
instance ToJSON BoardSize

instance Bounded BoardSize where
  minBound = BoardSize 0
  maxBound = BoardSize 26

instance Enum BoardSize where
  toEnum s = fromMaybe undefined $ boardSize s
  fromEnum s = getBoardSize s

-- | Create 'BoardSize'.
boardSize :: Int -> Maybe BoardSize
boardSize s
  | s > 0 && s <= 26 = Just $ BoardSize s
  | otherwise = Nothing

ppFull :: KnownNat n => BoardSquare n -> String
ppFull (BSquare s vec) = decNumbers ++ numbers ++ bStr
  where bStr = unlines $ zipWith (:) alphabet $ (lines . ppRaw) (BSquare s vec)
        numbers = " " ++ concatMap (show . (`mod` 10)) [ 1 .. fromEnum s ] ++ "\n"
        decNumbers = if fromEnum s >= 10 then decNumbersRaw else ""
        decNumbersRaw = " " ++ concatMap ((++ "         ") . show) [ 0 .. fromEnum s `div` 10 ] ++ "\n"
        alphabet = map ((toEnum :: Int -> Char) . (+ 96))  [ 1 .. fromEnum s ]

ppRaw :: forall n. KnownNat n => BoardSquare n -> String
ppRaw (BSquare s vec) = concatMap ppRow rows
  where ppRow = (++ "\n") . V.toList . V.map (renderStone :: Stone (PlayerN n) -> Char)
        rows = map slice [ i * fromEnum s | i <- [0..(fromEnum s - 1)] ] :: [V.Vector (Stone (PlayerN n))]
        slice n = V.slice n (fromEnum s) vec

instance KnownNat n => Board (BoardSquare n) Coord where
  empty config = do s <- boardSize $ size config
                    return . BSquare s $ V.replicate (fromEnum s * fromEnum s) Free

  coords (BSquare s _) = [ Coord x y | x <- range , y <- range ]
    where range = [ 0 .. (fromEnum s - 1) ]

  libertyCoords board (Coord x y) = filter (flip elem $ coords board) unsafeLibertyCoords
    where unsafeLibertyCoords = [ Coord (x-1) y
                                , Coord x     (y+1)
                                , Coord (x+1) y
                                , Coord x     (y-1)
                                ]

instance KnownNat n => Game (BoardSquare n) Coord n where
  getStone (BSquare s vec) (Coord x y) = vec V.! coordToVecInd s (Coord x y)

  putStone (BSquare s vec) coord stone = BSquare s newVec
    where newVec = V.update vec $ V.singleton (coordToVecInd s coord , stone)

instance KnownNat n => TermGame (BoardSquare n) Coord n where
  renderBoard = ppFull

  readCoord board str = if length wrds == 2
                        && charsInRange 48 57 x
                        && charsInRange 97 122 y
                        && length y == 1
                        then let coord = Coord xInt yInt
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

instance KnownNat n => JSONGame (BoardSquare n) Coord n
