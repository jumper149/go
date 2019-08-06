{-# LANGUAGE FunctionalDependencies #-}

module Board ( Player (..)
             , Stone (..)
             , Coord (..)
             , Board (..)
             , Gear (..)
             ) where

class (Eq p, Enum p, Bounded p) => Player p

class (Eq s, Player p) => Stone s p | s -> p where
  free :: s
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
  getStone :: b -> c -> s
  putStone :: b -> c -> s -> b
