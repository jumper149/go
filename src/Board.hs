{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Board ( Player (..)
             , Stone (..)
             , Board (..)
             , Gear ( getStone
                    , putStone
                    , chain
                    , chains
                    , hasLiberty
                    )
             ) where

import qualified Data.Set as S
import Data.List (nub)

class (Eq p, Enum p, Bounded p, Ord p) => Player p where
  char :: p -> Char

data Stone p = Free
             | Off
             | Stone p
  deriving (Eq, Ord)

instance Player p => Show (Stone p) where
  show Free = " "
  show Off = undefined
  show (Stone p) = [ char p ]

--stones :: Player p => [Stone p]
--stones = map Stone [ toEnum 0 .. ]

data Chain p c = Chain (Stone p) (S.Set c)
               | NoChain
  deriving (Eq, Ord)

class (Eq b, Eq c, Ord c) => Board b c | b -> c where
  empty :: b
  coords :: b -> [c]
  libertyCoords :: b -> c -> [c]

class (Board b c, Player p) => Gear b c p | b -> c where
  getStone :: b -> c -> Stone p
  putStone :: b -> c -> Stone p -> b

  accChain :: b -> Stone p -> c -> S.Set c -> S.Set c
  accChain board (Stone p) coord acc = S.singleton coord `S.union` newSames `S.union` recChain
    where newAcc = acc `S.union` S.singleton coord `S.union` newSames :: S.Set c
          newSames = S.fromList $ filter ((== Stone p) . getStone board) $ libertyCoords board coord :: S.Set c
          rec :: c -> S.Set c
          rec x = accChain board (Stone p) x newAcc
          recChain = S.unions $ S.map rec (newSames S.\\ acc) :: S.Set c
  accChain _ _ _ _ = S.empty

  chain :: b -> c -> Chain p c
  chain board coord
    | coords == S.empty = NoChain
    | otherwise = Chain stone coords
    where coords = accChain board stone coord S.empty :: S.Set c
          stone = getStone board coord :: Stone p

  chains :: b -> S.Set (Chain p c)
  chains board = S.filter (/= NoChain) $ S.map (chain board) $ S.fromList (coords board)

  hasLiberty :: b -> Chain p c -> Bool
  hasLiberty board (Chain stone coords) = S.foldr (||) False bools
    where bools = S.map ((== (Free :: Stone p)) . (getStone board :: c -> Stone p)) coords
