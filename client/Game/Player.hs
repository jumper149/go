module Game.Player ( Color (..)
                   , colorize
                   ) where

import Data.Char (isLower, toLower, toUpper)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import GHC.Generics
import GHC.TypeLits
import Miso.String (ToMisoString (..))

import Go.Game.Player

-- TODO: Make 'maxBound' infinite.
-- | Some colors.
data Color = Black
           | White
           | Red
           | Green
           | Blue
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

instance ToMisoString Color where
    toMisoString = toMisoString . fmap toLower . show

-- | Give a color to a player.
colorize :: forall n. KnownNat n => PlayerN n -> Color
colorize p = fromJust . M.lookup p . M.fromList $ zip players cycledColors
  where cycledColors = cycle [ minBound .. (maxBound :: Color) ]
        players = [ minBound .. (maxBound :: PlayerN n) ]
