{-# LANGUAGE FunctionalDependencies #-}

module Board ( Player (..)
             , Stone (..)
             , Coord (..)
             , Board (..)
             , Gear (..)
             ) where

--import Data.List (nub)

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

class Eq c => Coord c

class Eq b => Board b where
  empty :: b

class (Board b, Coord c, Player p) => Gear b c p | b -> c p where
  neighborCoords :: b -> c -> [c]
  libertyCoords :: b -> c -> [c]

  getStone :: b -> c -> Stone p
  putStone :: b -> c -> Stone p -> b

--  hasLiberty :: b -> c -> [(c,s)] -> Bool
--  hasLiberty board coord [] = True
--    where
--          potentialLiberties = zip potentialLiberties potentialLibertyStones :: [(c,s)]
--          potentialLibertyStones = map (getStone board) potentialLibertyCoords :: [s]
--          potentialLibertyCoords = nub $ libertyCoords board coord :: [c]
----          isChain :: (c,s) -> Bool
----          isChain (c , s) = chainStone == s
----          chainStone = getStone board coord :: s
