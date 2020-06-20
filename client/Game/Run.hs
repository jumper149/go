module Game.Run ( MisoGame (..)
                ) where

import GHC.TypeLits
import Miso.Html

import qualified Go.Run.JSON as G

import qualified Game.Board.Default as D
import Game.Operation

class G.JSONGame b c n => MisoGame b c n where
  viewBoard :: b -> Maybe c -> View (GameOperation b c n)

instance (KnownNat i, KnownNat n) => MisoGame (D.Board i n) (D.Coord i) n where
  viewBoard = D.viewBoard
