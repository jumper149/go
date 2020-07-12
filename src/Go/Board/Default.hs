module Go.Board.Default ( Board (..)
                        , Coord (..)
                        , packCoord
                        , getCoord
                        ) where

import Control.Applicative (liftA2)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.OrphanInstances ()
import Data.Bifunctor
import Data.Finite
import Data.Maybe (mapMaybe)
import Data.Proxy
import qualified Data.Vector.Sized as V
import GHC.Generics
import GHC.TypeLits

import Go.Game.Game
import Go.Game.Player
import Go.Run.JSON

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

instance KnownNat i => FromJSON (Coord i)
instance KnownNat i => ToJSON (Coord i)

packCoord :: KnownNat i => (Integer,Integer) -> Maybe (Coord i)
packCoord = fmap (uncurry Coord) . uncurry (liftA2 (,)) . bimap packFinite packFinite

getCoord :: KnownNat i => Coord i -> (Integer,Integer)
getCoord (Coord x y) = (getFinite x , getFinite y)

-- | Represents a square board. Contains a 'V.Vector' with all 'Stone's.
newtype Board (i :: Nat) n = Board (V.Vector i (V.Vector i (Stone (PlayerN n))))
  deriving (Eq, Generic, Ord, Read, Show)

instance (KnownNat i, KnownNat n) => FromJSON (Board i n) where
instance (KnownNat i, KnownNat n) => ToJSON (Board i n) where

instance KnownNat i => GameCoord (Coord i) where
  libertyCoords c = mapMaybe packCoord unsafeLibertyCoords
    where unsafeLibertyCoords = [ (cx-1 , cy  )
                                , (cx   , cy+1)
                                , (cx+1 , cy  )
                                , (cx   , cy-1)
                                ]
          (cx,cy) = getCoord c

instance (KnownNat i, KnownNat n) => Game (Board i n) where
  type AssociatedCoord (Board i n) = Coord i
  type AssociatedPlayerCount (Board i n) = n

  empty = Board . V.replicate . V.replicate $ Free

  getStone (Board grid) (Coord x y) = let row = V.index grid y
                                      in V.index row x

  putStone (Board grid) (Coord x y) stone = Board newGrid
    where newGrid = grid V.// [(y , newRow)]
          newRow = row V.// [(x , stone)]
          row = V.index grid y

instance (KnownNat i, KnownNat n) => JSONGame (Board i n)
