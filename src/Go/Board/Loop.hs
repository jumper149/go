module Go.Board.Loop ( Board (..)
                     , Coord (..)
                     , packCoord
                     , getCoord
                     ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Maybe (mapMaybe)
import Data.Proxy
import GHC.Generics
import GHC.TypeLits

import qualified Go.Board.Default as D
import Go.Game.Game
import Go.Run.JSON

newtype Coord i = Coord (D.Coord i)
  deriving (Bounded, Enum, Eq, FromJSON, Generic, Ord, Read, Show, ToJSON)

packCoord :: KnownNat i => (Integer,Integer) -> Maybe (Coord i)
packCoord = fmap Coord . D.packCoord

getCoord :: KnownNat i => Coord i -> (Integer,Integer)
getCoord (Coord coord) = D.getCoord coord

newtype Board i n = Board (D.Board i n)
  deriving (Eq, FromJSON, Generic, Ord, Read, Show, ToJSON)

instance KnownNat i => GameCoord (Coord i) where
  libertyCoords (Coord coord) = Coord <$> mapMaybe (D.packCoord . wrapX) unsafeLibertyCoords
    where unsafeLibertyCoords = [ (cx-1 , cy  )
                                , (cx   , cy+1)
                                , (cx+1 , cy  )
                                , (cx   , cy-1)
                                ]
          wrapX (x,y) = (x `mod` s , y)
          s = natVal (Proxy :: Proxy i)
          (cx,cy) = D.getCoord coord

instance (KnownNat i, KnownNat n) => Game (Board i n) where
  type AssociatedCoord (Board i n) = Coord i
  type AssociatedPlayerCount (Board i n) = n

  empty = Board empty

  getStone (Board board) (Coord coord) = getStone board coord

  putStone (Board board) (Coord coord) stone = Board $ putStone board coord stone

instance (KnownNat i, KnownNat n) => JSONGame (Board i n)
