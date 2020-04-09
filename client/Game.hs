module Game ( MisoGame (..)
            ) where

import GHC.TypeLits
import Miso.Html

import qualified Go.Game.Game as G

import qualified Board.Default as D
import Operation

class G.Game b c n => MisoGame b c n where
  viewBoard :: b -> Maybe c -> View (Operation c)

instance KnownNat n => MisoGame (D.BoardSquare n) D.Coord n where
  viewBoard = D.viewBoard
