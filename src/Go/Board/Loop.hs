module Go.Board.Loop ( BoardLoop (..)
                     , CoordXY (..)
                     , PlayerBW (..)
                     ) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

import Go.Board.Default
import Go.Game.Config
import Go.Game.Game
import Go.Run.Server.JSON
import Go.Run.Term

newtype BoardLoop = BLoop BoardSquare
  deriving (Eq, Generic)

instance FromJSON BoardLoop
instance ToJSON BoardLoop

instance Show BoardLoop where
  show (BLoop board) = show board

instance Board BoardLoop CoordXY where
  empty = fmap BLoop . (empty :: Config -> Maybe BoardSquare)

  coords (BLoop board) = coords board

  libertyCoords (BLoop (BSquare size vec)) (XY x y) = filter (flip elem $ coords (BLoop (BSquare size vec))) $ map wrap unsafeLibertyCoords
    where unsafeLibertyCoords = [ XY (x-1) y
                                , XY x     (y+1)
                                , XY (x+1) y
                                , XY x     (y-1)
                                ]
          wrap :: CoordXY -> CoordXY
          wrap (XY a b) = XY (a `mod` fromEnum size) b

instance Game BoardLoop CoordXY PlayerBW where
  getStone (BLoop board) = getStone board

  putStone (BLoop board) coord stone = BLoop $ putStone board coord stone

instance TermGame BoardLoop CoordXY PlayerBW where
  readCoord (BLoop board) = readCoord board

instance JSONGame BoardLoop CoordXY PlayerBW
