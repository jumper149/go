module Board ( Stone
             ) where

class Eq s => Stone s where
  freePoint :: s
  stones :: [s]
  territories :: [s]

class Eq b => Board b  where
