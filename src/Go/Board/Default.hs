{-# LANGUAGE KindSignatures #-}

module Go.Board.Default ( BoardSquare (..)
                        , Coord (..)
                        , mkCoord
                        ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Proxy
import qualified Data.Vector as V -- TODO: change for vector-sized
import GHC.Generics
import GHC.TypeLits

import Go.Game.Game
import Go.Game.Player
import Go.Run.JSON
import Go.Run.Term

-- | Represents the coordinates of a point on the board. Holds the x- and y-coordinate.
-- Coordinates are integers in the interval [0, boardsize).
data Coord (i :: Nat) = Coord { getX :: Int
                              , getY :: Int
                              }
  deriving (Eq, Generic, Ord, Read, Show) -- TODO: nice Bounded instance, maybe Enum too?

instance KnownNat i => FromJSON (Coord i)
instance KnownNat i => ToJSON (Coord i)

-- | Smart constructor where coordinates are required to be in the interval [1..boardsize].
mkCoord :: forall i. KnownNat i => Int -> Int -> Maybe (Coord i)
mkCoord x y = if check x && check y
                   then Just $ Coord (x-1) (y-1)
                   else Nothing
  where check c = c <= s && c >= 1
        s = fromEnum $ natVal (Proxy :: Proxy i)

-- | Transform coordinate to index to access the array of points on the board.
coordToVecInd :: forall i. KnownNat i => Coord i -> Int
coordToVecInd c = getX c + s * getY c
  where s = fromEnum $ natVal (Proxy :: Proxy i)

-- | Represents a square board. Contains the 'Coord' and a 'V.Vector' with all coordinates.
data BoardSquare (i :: Nat) n = BSquare (V.Vector (Stone (PlayerN n)))
  deriving (Eq, Generic, Ord, Read, Show)

instance (KnownNat i, KnownNat n) => FromJSON (BoardSquare i n)
instance (KnownNat i, KnownNat n) => ToJSON (BoardSquare i n)

ppFull :: forall i n. (KnownNat i, KnownNat n) => BoardSquare i n -> String
ppFull (BSquare vec) = decNumbers ++ numbers ++ bStr
  where bStr = unlines $ zipWith (:) alphabet $ (lines . ppRaw) (BSquare vec :: BoardSquare i n)
        numbers = " " ++ concatMap (show . (`mod` 10)) [ 1 .. s ] ++ "\n"
        decNumbers = if s >= 10 then decNumbersRaw else ""
        decNumbersRaw = " " ++ concatMap ((++ "         ") . show) [ 0 .. s `div` 10 ] ++ "\n"
        alphabet = map ((toEnum :: Int -> Char) . (+ 96))  [ 1 .. s ]
        s = fromEnum $ natVal (Proxy :: Proxy i)

ppRaw :: forall i n. (KnownNat i, KnownNat n) => BoardSquare i n -> String
ppRaw (BSquare vec) = concatMap ppRow rows
  where ppRow = (++ "\n") . V.toList . V.map (renderStone :: Stone (PlayerN n) -> Char)
        rows = map slice [ i * s | i <- [0..(s-1)] ] :: [V.Vector (Stone (PlayerN n))]
        slice n = V.slice n s vec
        s = fromEnum $ natVal (Proxy :: Proxy i)

instance (KnownNat i, KnownNat n) => Board (BoardSquare i n) (Coord i) where
  empty = BSquare $ V.replicate (s * s) Free
    where s = fromEnum $ natVal (Proxy :: Proxy i)

  coords (BSquare _) = [ Coord x y | x <- range , y <- range ]
    where range = [ 0 .. (s - 1) ]
          s = fromEnum $ natVal (Proxy :: Proxy i)

  libertyCoords board (Coord x y) = filter (flip elem $ coords board) unsafeLibertyCoords
    where unsafeLibertyCoords = [ Coord (x-1) y
                                , Coord x     (y+1)
                                , Coord (x+1) y
                                , Coord x     (y-1)
                                ]

instance (KnownNat i, KnownNat n) => Game (BoardSquare i n) (Coord i) n where
  getStone (BSquare vec) c = vec V.! coordToVecInd c

  putStone (BSquare vec) coord stone = BSquare newVec
    where newVec = V.update vec $ V.singleton (coordToVecInd coord , stone)

instance (KnownNat i, KnownNat n) => TermGame (BoardSquare i n) (Coord i) n where
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

instance (KnownNat i, KnownNat n) => JSONGame (BoardSquare i n) (Coord i) n
