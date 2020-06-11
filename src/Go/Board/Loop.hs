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
import Go.Run.Term

newtype Board i n = Board (D.Board i n)
  deriving (Eq, FromJSON, Generic, Ord, Read, Show, ToJSON)

instance (KnownNat i, KnownNat n) => GameBoard (Board i n) (D.Coord i) where
  empty = Board empty

  coords (Board board) = coords board

  libertyCoords _ c = mapMaybe (D.packCoord . wrapX) unsafeLibertyCoords
    where unsafeLibertyCoords = [ (cx-1 , cy  )
                                , (cx   , cy+1)
                                , (cx+1 , cy  )
                                , (cx   , cy-1)
                                ]
          wrapX (x,y) = (x `mod` s , y)
          s = natVal (Proxy :: Proxy i)
          (cx,cy) = D.getCoord c

instance (KnownNat i, KnownNat n) => Game (Board i n) (D.Coord i) n where
  getStone (Board board) = getStone board

  putStone (Board board) coord stone = Board $ putStone board coord stone

instance (KnownNat i, KnownNat n) => TermGame (Board i n) (D.Coord i) n where
  renderBoard (Board board) = renderBoard board

  readCoord (Board board) = readCoord board

instance (KnownNat i, KnownNat n) => JSONGame (Board i n) (D.Coord i) n
