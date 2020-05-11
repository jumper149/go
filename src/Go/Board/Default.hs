{-# LANGUAGE KindSignatures #-}

module Go.Board.Default ( BoardSquare (..)
                        , Coord (..) -- TODO: read-only?
                        , mkCoord
                        ) where

import Data.Aeson
import Data.Maybe (fromJust)
import Data.Proxy
import qualified Data.Vector.Sized as V
import GHC.Generics
import GHC.TypeLits

import Go.Game.Game
import Go.Game.Player
import Go.Run.JSON
import Go.Run.Term

-- | Represents the coordinates of a point on the board. Holds the x- and y-coordinate.
-- Coordinates are integers in the interval [0, i).
data Coord (i :: Nat) = Coord { getX :: Int
                              , getY :: Int
                              }
  deriving (Eq, Generic, Ord, Read, Show)

instance KnownNat i => Bounded (Coord i) where
  minBound = Coord lo lo
    where lo = 0
  maxBound = Coord hi hi
    where hi = fromEnum (natVal (Proxy :: Proxy i)) - 1

instance KnownNat i => Enum (Coord i) where
  toEnum j = Coord x y
    where (y,x) = divMod j s
          s = fromEnum $ natVal (Proxy :: Proxy i)
  fromEnum (Coord x y) = y * s + x
    where s = fromEnum $ natVal (Proxy :: Proxy i)

instance KnownNat i => FromJSON (Coord i)
instance KnownNat i => ToJSON (Coord i)

-- | Smart constructor where coordinates are required to be in the interval [1..i).
mkCoord :: forall i. KnownNat i => Int -> Int -> Maybe (Coord i)
mkCoord x y = if check x && check y
                   then Just $ Coord (x-1) (y-1)
                   else Nothing
  where check c = c <= s && c >= 1
        s = fromEnum $ natVal (Proxy :: Proxy i)

-- | Represents a square board. Contains a 'V.Vector' with all 'Stone's.
newtype BoardSquare (i :: Nat) n = BSquare (V.Vector i (V.Vector i (Stone (PlayerN n))))
  deriving (Eq, Generic, Ord, Read, Show)

-- TODO: orphan instances
instance (KnownNat n, FromJSON a) => FromJSON (V.Vector n a) where
  parseJSON = fmap (fromJust . V.toSized) . parseJSON
instance (KnownNat n, ToJSON a) => ToJSON (V.Vector n a) where
  toJSON = toJSON . V.fromSized
instance (KnownNat i, KnownNat n) => FromJSON (BoardSquare i n) where
instance (KnownNat i, KnownNat n) => ToJSON (BoardSquare i n) where

ppFull :: forall i n. (KnownNat i, KnownNat n) => BoardSquare i n -> String
ppFull (BSquare vec) = decNumbers ++ numbers ++ bStr
  where bStr = unlines $ zipWith (:) alphabet $ (lines . ppRaw) (BSquare vec :: BoardSquare i n)
        numbers = " " ++ concatMap (show . (`mod` 10)) [ 1 .. s ] ++ "\n"
        decNumbers = if s >= 10 then decNumbersRaw else ""
        decNumbersRaw = " " ++ concatMap ((++ "         ") . show) [ 0 .. s `div` 10 ] ++ "\n"
        alphabet = map ((toEnum :: Int -> Char) . (+ 96))  [ 1 .. s ]
        s = fromEnum $ natVal (Proxy :: Proxy i)

ppRaw :: forall i n. (KnownNat i, KnownNat n) => BoardSquare i n -> String
ppRaw (BSquare grid) = concatMap ppRow grid
  where ppRow = (++ "\n") . V.toList . V.map renderStone

instance (KnownNat i, KnownNat n) => Board (BoardSquare i n) (Coord i) where
  empty = BSquare . V.replicate . V.replicate $ Free

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
  getStone (BSquare grid) (Coord x y) = let row = V.unsafeIndex grid y -- TODO: dont use unsafe
                                        in V.unsafeIndex row x -- TODO

  putStone (BSquare grid) (Coord x y) stone = BSquare newGrid
    where newGrid = V.update grid $ V.singleton (y , newRow)
          newRow = V.update row $ V.singleton (x , stone)
          row = V.unsafeIndex grid y -- TODO: unsafe

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
