module Go.Board.Loop ( BoardLoop (..)
                     ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Proxy
import GHC.Generics
import GHC.TypeLits

import qualified Go.Board.Default as D
import Go.Game.Game
import Go.Run.JSON
import Go.Run.Term

newtype BoardLoop i n = BLoop (D.BoardSquare i n)
  deriving (Eq, FromJSON, Generic, Ord, Read, Show, ToJSON)

instance (KnownNat i, KnownNat n) => Board (BoardLoop i n) (D.Coord i) where
  empty = BLoop empty

  coords (BLoop board) = coords board

  libertyCoords (BLoop (D.BSquare vec)) (D.Coord x y) = filter (flip elem $ coords (BLoop (D.BSquare vec))) $ map wrap unsafeLibertyCoords
    where unsafeLibertyCoords = [ D.Coord (x-1) y
                                , D.Coord x     (y+1)
                                , D.Coord (x+1) y
                                , D.Coord x     (y-1)
                                ]
          wrap c = D.Coord (D.getX c `mod` s) (D.getY c)
          s = fromEnum $ natVal (Proxy :: Proxy i)

instance (KnownNat i, KnownNat n) => Game (BoardLoop i n) (D.Coord i) n where
  getStone (BLoop board) = getStone board

  putStone (BLoop board) coord stone = BLoop $ putStone board coord stone

instance (KnownNat i, KnownNat n) => TermGame (BoardLoop i n) (D.Coord i) n where
  renderBoard (BLoop board) = renderBoard board

  readCoord (BLoop board) = readCoord board

instance (KnownNat i, KnownNat n) => JSONGame (BoardLoop i n) (D.Coord i) n
