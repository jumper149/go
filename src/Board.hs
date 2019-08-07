{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Board ( Player (..)
             , Stone (..)
             , Board (..)
             , Gear (..)
             ) where

import Data.List (nub)

class (Eq p, Enum p) => Player p where
  char :: p -> Char

data Stone p = Free
             | Off
             | Stone p
  deriving Eq

instance Player p => Show (Stone p) where
  show Free = " "
  show Off = undefined
  show (Stone p) = [ char p ]

stones :: Player p => [Stone p]
stones = map Stone [ toEnum 0 .. ]

class (Eq b, Eq c) => Board b c where
  empty :: b
  neighborCoords :: b -> c -> [c]
  libertyCoords :: b -> c -> [c]

class (Board b c, Player p) => Gear b c p where
  getStone :: b -> c -> Stone p
  putStone :: b -> c -> Stone p -> b

  hasLiberty :: b -> c -> [(c,Stone p)] -> Bool
  hasLiberty board coord [] = True
    where
          potentialLiberties = zip potentialLibertyCoords potentialLibertyStones :: [(c,Stone p)]
          potentialLibertyStones = map (getStone board) potentialLibertyCoords :: [Stone p]
          potentialLibertyCoords = nub $ libertyCoords board coord :: [c]
--          isChain :: (c,s) -> Bool
--          isChain (c , s) = chainStone == s
--          chainStone = getStone board coord :: s
