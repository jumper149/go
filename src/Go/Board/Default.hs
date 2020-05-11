{-# LANGUAGE KindSignatures #-}

module Go.Board.Default ( Board (..)
                        , Coord (..) -- TODO: read-only?
                        , packCoord
                        , getCoord
                        ) where

import Control.Applicative (liftA2)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Bifunctor
import Data.Finite
import Data.Maybe (catMaybes, fromJust)
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
data Coord (i :: Nat) = Coord { getX :: Finite i -- TODO: turn X and Y around for Ord to be nicer
                              , getY :: Finite i
                              }
  deriving (Bounded, Eq, Generic, Ord, Read, Show)

instance KnownNat i => Enum (Coord i) where
  toEnum j = Coord (finite $ toEnum x) (finite $ toEnum y)
    where (y,x) = divMod j s
          s = fromEnum $ natVal (Proxy :: Proxy i)
  fromEnum (Coord x y) = fromEnum $ getFinite y * s + getFinite x
    where s = natVal (Proxy :: Proxy i)

instance KnownNat n => FromJSON (Finite n) where
  parseJSON = fmap finite . parseJSON
instance KnownNat n => ToJSON (Finite n) where
  toJSON = toJSON . getFinite
instance KnownNat i => FromJSON (Coord i)
instance KnownNat i => ToJSON (Coord i)

packCoord :: KnownNat i => (Integer,Integer) -> Maybe (Coord i)
packCoord = fmap (uncurry Coord) . uncurry (liftA2 (,)) . bimap packFinite packFinite

getCoord :: KnownNat i => Coord i -> (Integer,Integer)
getCoord (Coord x y) = (getFinite x , getFinite y)

-- | Represents a square board. Contains a 'V.Vector' with all 'Stone's.
newtype Board (i :: Nat) n = Board (V.Vector i (V.Vector i (Stone (PlayerN n))))
  deriving (Eq, Generic, Ord, Read, Show)

-- TODO: orphan instances
instance (KnownNat n, FromJSON a) => FromJSON (V.Vector n a) where
  parseJSON = fmap (fromJust . V.toSized) . parseJSON
instance (KnownNat n, ToJSON a) => ToJSON (V.Vector n a) where
  toJSON = toJSON . V.fromSized
instance (KnownNat i, KnownNat n) => FromJSON (Board i n) where
instance (KnownNat i, KnownNat n) => ToJSON (Board i n) where

ppFull :: forall i n. (KnownNat i, KnownNat n) => Board i n -> String
ppFull (Board vec) = decNumbers ++ numbers ++ bStr
  where bStr = unlines $ zipWith (:) alphabet $ (lines . ppRaw) (Board vec :: Board i n)
        numbers = " " ++ concatMap (show . (`mod` 10)) [ 1 .. s ] ++ "\n"
        decNumbers = if s >= 10 then decNumbersRaw else ""
        decNumbersRaw = " " ++ concatMap ((++ "         ") . show) [ 0 .. s `div` 10 ] ++ "\n"
        alphabet = map ((toEnum :: Int -> Char) . (+ 96))  [ 1 .. s ]
        s = fromEnum $ natVal (Proxy :: Proxy i)

ppRaw :: forall i n. (KnownNat i, KnownNat n) => Board i n -> String
ppRaw (Board grid) = concatMap ppRow grid
  where ppRow = (++ "\n") . V.toList . V.map renderStone

instance (KnownNat i, KnownNat n) => GameBoard (Board i n) (Coord i) where
  empty = Board . V.replicate . V.replicate $ Free

  coords (Board _) = [ minBound .. maxBound ]

  libertyCoords _ c = catMaybes $ map packCoord unsafeLibertyCoords
    where unsafeLibertyCoords = [ (cx-1 , cy  )
                                , (cx   , cy+1)
                                , (cx+1 , cy  )
                                , (cx   , cy-1)
                                ]
          (cx,cy) = getCoord c

instance (KnownNat i, KnownNat n) => Game (Board i n) (Coord i) n where
  getStone (Board grid) (Coord x y) = let row = V.index grid y
                                      in V.index row x

  putStone (Board grid) (Coord x y) stone = Board newGrid
    where newGrid = grid V.// [(y , newRow)]
          newRow = row V.// [(x , stone)]
          row = V.index grid y

instance (KnownNat i, KnownNat n) => TermGame (Board i n) (Coord i) n where
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
          x = head wrds
          y = head $ tail wrds
          xInt = toEnum $ read x - 1
          yInt = toEnum $ fromEnum (head y) - fromEnum 'a'
          charsInRange :: Int -> Int -> String -> Bool
          charsInRange lo hi st = and bools
            where nums = map fromEnum st
                  bools = map (\ i -> i >= lo && i <= hi) nums

instance (KnownNat i, KnownNat n) => JSONGame (Board i n) (Coord i) n
