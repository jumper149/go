module Game.Player ( Color (..)
                   , colorize
                   ) where

import qualified Data.Map as M
import Data.Maybe (fromJust)
import GHC.TypeLits

import Go.Game.Player

import Color

-- | Give a color to a player.
colorize :: forall n. KnownNat n => PlayerN n -> Color
colorize p = fromJust . M.lookup p . M.fromList $ zip players cycledColors
  where cycledColors = cycle [ Black Dark
                             , White Light
                             , Red Dark
                             , Blue Dark
                             , Yellow Dark
                             , Magenta Dark
                             , Cyan Dark
                             , Red Light
                             , Blue Light
                             , Yellow Light
                             , Magenta Light
                             , Cyan Light
                             ]
        players = [ minBound .. (maxBound :: PlayerN n) ]
