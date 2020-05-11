module Go.Board.Loop ( Board (..)
                     , D.Coord (..)
                     , D.mkCoord
                     ) where

import Data.Aeson (FromJSON, ToJSON)
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

  libertyCoords (Board (D.Board vec)) (D.Coord x y) = filter (flip elem $ coords (Board (D.Board vec))) $ map wrap unsafeLibertyCoords
    where unsafeLibertyCoords = [ D.Coord (x-1) y
                                , D.Coord x     (y+1)
                                , D.Coord (x+1) y
                                , D.Coord x     (y-1)
                                ]
          wrap c = D.Coord (D.getX c `mod` s) (D.getY c)
          s = fromEnum $ natVal (Proxy :: Proxy i)

instance (KnownNat i, KnownNat n) => Game (Board i n) (D.Coord i) n where
  getStone (Board board) = getStone board

  putStone (Board board) coord stone = Board $ putStone board coord stone

instance (KnownNat i, KnownNat n) => TermGame (Board i n) (D.Coord i) n where
  renderBoard (Board board) = renderBoard board

  readCoord (Board board) = readCoord board

instance (KnownNat i, KnownNat n) => JSONGame (Board i n) (D.Coord i) n
