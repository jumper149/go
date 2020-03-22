{-# LANGUAGE DeriveGeneric, FunctionalDependencies #-}

module Game ( Game ( getStone
                   , putStone
                   )
            , updateBoard
            , Board (..)
            , Player
            , Stone (..)
            ) where

import qualified Data.Set as S

import GHC.Generics
import Data.Aeson

-- | The states of a spot for a stone are represented by this data type.
data Stone p = Free
             | Stone p
  deriving (Eq, Ord, Generic)

instance (Generic p, FromJSON p) => FromJSON (Stone p)
instance (Generic p, ToJSON p) => ToJSON (Stone p)

-- | Stones placed on coordinates can form chains which are represented by this data type.
data Chain p c = Chain (Stone p) (S.Set c)
  deriving (Eq, Ord)

-- | Check if a coordinate is part of chain. Does not check if stones are matching.
partOfChain :: Ord c => c -> Chain p c -> Bool
partOfChain c (Chain _ cs) = c `S.member` cs

-- | Remove chains of free stones and put them in an ordered list, that is cycled, so that the
-- chains of the given player are last.
prepChains :: Player p => p -> S.Set (Chain p c) -> [Chain p c]
prepChains player = swapJoin . split . removeFrees
  where removeFrees = S.filter $ \ (Chain stone _) -> stone /= Free
        split = S.spanAntitone (\ (Chain stone _) -> stone <= Stone player)
        swapJoin (a,b) = S.toAscList b <> S.toAscList a

class (Eq p, Enum p, Bounded p, Ord p) => Player p

class (Eq b, Eq c, Ord c) => Board b c | b -> c where

  -- | Return an empty board.
  empty :: b

  -- | Return a list of all coords covering the board.
  coords :: b -> [c]

  -- | Return a list of all adjacent coordinates.
  libertyCoords :: b -> c -> [c]

class (Board b c, Player p) => Game b c p | b -> c p where

  -- | Returns the stone.
  getStone :: b -> c -> Stone p

  -- | Places a stone on a coordinate and returns the board.
  putStone :: b -> c -> Stone p -> b

accChain :: forall b c p. Game b c p => b -> Stone p -> c -> S.Set c -> S.Set c
accChain board stone coord acc = foldr (accChain board stone) newAcc neededSames
  where newAcc = acc `S.union` sames
        neededSames = sames `S.difference` acc
        sames = S.filter ((== stone) . getStone board) libs
        libs = S.fromList $ libertyCoords board coord

chain :: forall b c p. Game b c p => b -> c -> Chain p c
chain board coord = Chain stone crds
  where stone = getStone board coord
        crds = accChain board stone coord acc
        acc = S.singleton coord

accChains :: forall b c p. Game b c p => b -> S.Set (Chain p c)
accChains board = foldr addCoord S.empty $ coords board
  where addCoord crd chns = if any (partOfChain crd) chns
                               then chns
                               else chain board crd `S.insert` chns

hasLiberty :: forall b c p. Game b c p => b -> Chain p c -> Bool
hasLiberty board (Chain _ crds) = or bools
  where bools = S.map ((== Free) . (getStone board :: c -> Stone p)) libs
        libs = S.unions $ S.map (S.fromList . libertyCoords board) crds

removeChain :: forall b c p. Game b c p => b -> Chain p c -> b
removeChain board (Chain _ crds) = foldl putFree board crds
  where putFree brd crd = putStone brd crd Free

removeSurrounded :: forall b c p. Game b c p => b -> Chain p c -> b
removeSurrounded brd chn = if hasLiberty brd chn
                              then brd
                              else removeChain brd chn

-- | Remove chains without liberties. Give player as an argument to solve atari. Return board.
updateBoard :: forall b c p. Game b c p => b -> p -> b
updateBoard board player = foldl removeSurrounded board chains
  where chains = prepChains player $ accChains board
