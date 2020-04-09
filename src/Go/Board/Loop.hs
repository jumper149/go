module Go.Board.Loop ( BoardLoop (..)
                     ) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import GHC.TypeLits

import qualified Go.Board.Default as D
import Go.Game.Config
import Go.Game.Game
import Go.Run.JSON
import Go.Run.Term

newtype BoardLoop n = BLoop (D.BoardSquare n)
  deriving (Eq, FromJSON, Generic, Ord, Read, Show, ToJSON)

instance KnownNat n => Board (BoardLoop n) D.Coord where
  empty = fmap BLoop . (empty :: Config -> Maybe (D.BoardSquare n))

  coords (BLoop board) = coords board

  libertyCoords (BLoop (D.BSquare s vec)) (D.Coord x y) = filter (flip elem $ coords (BLoop (D.BSquare s vec))) $ map wrap unsafeLibertyCoords
    where unsafeLibertyCoords = [ D.Coord (x-1) y
                                , D.Coord x     (y+1)
                                , D.Coord (x+1) y
                                , D.Coord x     (y-1)
                                ]
          wrap c = D.Coord (D.getX c `mod` fromEnum s) (D.getY c)

instance KnownNat n => Game (BoardLoop n) D.Coord n where
  getStone (BLoop board) = getStone board

  putStone (BLoop board) coord stone = BLoop $ putStone board coord stone

instance KnownNat n => TermGame (BoardLoop n) D.Coord n where
  renderBoard (BLoop board) = renderBoard board

  readCoord (BLoop board) = readCoord board

instance KnownNat n => JSONGame (BoardLoop n) D.Coord n
