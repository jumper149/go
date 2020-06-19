module Go.Board.Loop ( Board (..)
                     , D.Coord (..)
                     , D.packCoord
                     , D.getCoord
                     ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Maybe (mapMaybe)
import Data.Proxy
import GHC.Generics
import GHC.TypeLits

import qualified Go.Board.Default as D
import Go.Game.Game
import Go.Run.JSON

newtype Board i n = Board (D.Board i n)
  deriving (Eq, FromJSON, Generic, Ord, Read, Show, ToJSON)

newtype Coord i = Coord (D.Coord i)
  deriving (Bounded, Enum, Eq, FromJSON, Generic, Ord, Read, Show, ToJSON)

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

instance (KnownNat i, KnownNat n) => Game (Board i n) (Coord i) n where
  empty = Board empty

  getStone (Board board) (Coord coord) = getStone board coord

  putStone (Board board) (Coord coord) stone = Board $ putStone board coord stone

instance (KnownNat i, KnownNat n) => JSONGame (Board i n) (Coord i) n
