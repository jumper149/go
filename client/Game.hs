module Game ( MisoGame (..)
            ) where

import GHC.TypeLits
import Miso.Html

import qualified Go.Run.JSON as G

import qualified Board.Default as D
import Operation

class G.JSONGame b c n => MisoGame b c n where
  viewBoard :: b -> Maybe c -> View (Operation b c n)

instance (KnownNat i, KnownNat n) => MisoGame (D.Board i n) (D.Coord i) n where
  viewBoard = D.viewBoard
