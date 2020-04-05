module Go.Board.Loop ( BoardLoop (..)
                     ) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import GHC.TypeLits

import Go.Board.Default
import Go.Game.Config
import Go.Game.Game
import Go.Run.Server.JSON
import Go.Run.Term

newtype BoardLoop n = BLoop (BoardSquare n)
  deriving (Eq, FromJSON, Generic, Ord, Read, Show, ToJSON)

instance KnownNat n => Board (BoardLoop n) CoordXY where
  empty = fmap BLoop . (empty :: Config -> Maybe (BoardSquare n))

  coords (BLoop board) = coords board

  libertyCoords (BLoop (BSquare s vec)) (XY x y) = filter (flip elem $ coords (BLoop (BSquare s vec))) $ map wrap unsafeLibertyCoords
    where unsafeLibertyCoords = [ XY (x-1) y
                                , XY x     (y+1)
                                , XY (x+1) y
                                , XY x     (y-1)
                                ]
          wrap :: CoordXY -> CoordXY
          wrap (XY a b) = XY (a `mod` fromEnum s) b

instance KnownNat n => Game (BoardLoop n) CoordXY n where
  getStone (BLoop board) = getStone board

  putStone (BLoop board) coord stone = BLoop $ putStone board coord stone

instance KnownNat n => TermGame (BoardLoop n) CoordXY n where
  renderBoard (BLoop board) = renderBoard board

  readCoord (BLoop board) = readCoord board

instance KnownNat n => JSONGame (BoardLoop n) CoordXY n
