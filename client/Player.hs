module Player ( Color (..)
              , colorize
              ) where

import Data.Char (isLower, toLower, toUpper)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import GHC.Generics
import GHC.TypeLits
import qualified Miso.String as MisoS

import Go.Game.Player

-- TODO: Make 'maxBound' infinite.
-- | Some colors.
data Color = Black
           | White
           | Red
           | Green
           | Blue
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

instance MisoS.ToMisoString Color where
    toMisoString = MisoS.toMisoString . fmap toLower . show

    -- TODO: Unnecessary function
    fromMisoString mstr = case MisoS.fromMisoString mstr of
                            "" -> undefined
                            c:cs -> if isLower c
                                       then read $ toUpper c : cs
                                       else undefined

-- | Give a color to a player.
colorize :: forall n. KnownNat n => PlayerN n -> Color
colorize p = fromJust . M.lookup p . M.fromList $ zip players cycledColors
  where cycledColors = cycle [ minBound .. (maxBound :: Color) ]
        players = [ minBound .. (maxBound :: PlayerN n) ]
