module Rules ( Rules
             , Permission
             , KoRule
             ) where

import Game

data Permission = Allowed
                | Forbidden
                deriving (Eq, Show)

data KoRule = KoRule Permission
            | SuperKo
            deriving (Eq, Show)

data Komi = Integral Int
          | PlusHalf Int
          deriving (Eq, Show)

data Rules = Rules { suicide :: Permission
                   , ko :: KoRule
                   }
