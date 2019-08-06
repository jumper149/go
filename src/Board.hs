{-# LANGUAGE FunctionalDependencies #-}

module Board ( Player (..)
             , Stone (..)
             , Coord (..)
             , Board (..)
             , Gear (..)
             ) where

--import Data.List (nub)

class (Eq p, Enum p, Bounded p) => Player p

class (Eq s, Player p) => Stone s p | s -> p where
  free :: s
  off :: s
  stone :: p -> s
  stones :: [s]
  stones = map stone [ minBound .. ]
  territory :: p -> s
  territories :: [s]
  territories = map territory [ minBound .. ]

class Eq c => Coord c

class Eq b => Board b

class (Board b, Coord c, Stone s p) => Gear b c s p | b -> c s where

  empty :: b

  neighborCoords :: b -> c -> [c]
  libertyCoords :: b -> c -> [c]

  getStone :: b -> c -> s
  putStone :: b -> c -> s -> b

--  hasLiberty :: b -> c -> [(c,s)] -> Bool
--  hasLiberty board coord [] = True
--    where
--          potentialLiberties = zip potentialLiberties potentialLibertyStones :: [(c,s)]
--          potentialLibertyStones = map (getStone board) potentialLibertyCoords :: [s]
--          potentialLibertyCoords = nub $ libertyCoords board coord :: [c]
----          isChain :: (c,s) -> Bool
----          isChain (c , s) = chainStone == s
----          chainStone = getStone board coord :: s
