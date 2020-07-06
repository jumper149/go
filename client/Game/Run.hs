module Game.Run ( MisoGame (..)
                ) where

import GHC.TypeLits
import Miso.Html

import qualified Go.Game.Game as G
import qualified Go.Run.JSON as G

import qualified Game.Board.Default as D
import Game.Operation

class G.JSONGame b => MisoGame b where
  viewBoard :: b -> Maybe (G.AssociatedCoord b) -> View (GameOperation b)

instance (KnownNat i, KnownNat n) => MisoGame (D.Board i n) where
  viewBoard = D.viewBoard
