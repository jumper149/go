{-# LANGUAGE FunctionalDependencies, KindSignatures #-}

module Go.Game.Game ( Game ( getStone
                           , putStone
                           )
                    , updateBoard
                    , Board (..)
                    , Stone (..)
                    ) where


import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Set as S
import GHC.Generics (Generic)
import GHC.TypeLits

import Go.Game.Config
import Go.Game.Player

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
partOfChain :: (Ord c, KnownNat n) => c -> Chain (PlayerN n) c -> Bool
partOfChain c (Chain _ cs) = c `S.member` cs

-- | Remove chains of free stones and put them in an ordered list, that is cycled, so that the
-- chains of the given player are last.
prepChains :: KnownNat n => PlayerN n -> S.Set (Chain (PlayerN n) c) -> [Chain (PlayerN n) c]
prepChains player = swapJoin . split . removeFrees
  where removeFrees = S.filter $ \ (Chain stone _) -> stone /= Free
        split = S.spanAntitone (\ (Chain stone _) -> stone <= Stone player)
        swapJoin (a,b) = S.toAscList b <> S.toAscList a

class (Eq b, Eq c, Ord c) => Board b c | b -> c where

  -- | Return an empty board.
  empty :: Config -> Maybe b

  -- | Return a list of all coords covering the board.
  coords :: b -> [c]

  -- | Return a list of all adjacent coordinates.
  libertyCoords :: b -> c -> [c]

class (Board b c, KnownNat n) => Game b c n | b -> c n where

  -- | Returns the stone.
  getStone :: b -> c -> Stone (PlayerN n)

  -- | Places a stone on a coordinate and returns the board.
  putStone :: b -> c -> Stone (PlayerN n) -> b

accChain :: forall b c n. Game b c n => b -> Stone (PlayerN n) -> c -> S.Set c -> S.Set c
accChain board stone coord acc = foldr (accChain board stone) newAcc neededSames
  where newAcc = acc `S.union` sames
        neededSames = sames `S.difference` acc
        sames = S.filter ((== stone) . getStone board) libs
        libs = S.fromList $ libertyCoords board coord

chain :: forall b c n. Game b c n => b -> c -> Chain (PlayerN n) c
chain board coord = Chain stone crds
  where stone = getStone board coord
        crds = accChain board stone coord acc
        acc = S.singleton coord

accChains :: forall b c n. Game b c n => b -> S.Set (Chain (PlayerN n) c)
accChains board = foldr addCoord S.empty $ coords board
  where addCoord crd chns = if any (partOfChain crd) chns
                               then chns
                               else chain board crd `S.insert` chns

hasLiberty :: forall b c n. Game b c n => b -> Chain (PlayerN n) c -> Bool
hasLiberty board (Chain _ crds) = or bools
  where bools = S.map ((== Free) . (getStone board :: c -> Stone (PlayerN n))) libs
        libs = S.unions $ S.map (S.fromList . libertyCoords board) crds

removeChain :: forall b c n. Game b c n => b -> Chain (PlayerN n) c -> b
removeChain board (Chain _ crds) = foldl putFree board crds
  where putFree brd crd = putStone brd crd Free

removeSurrounded :: forall b c n. Game b c n => b -> Chain (PlayerN n) c -> b
removeSurrounded brd chn = if hasLiberty brd chn
                              then brd
                              else removeChain brd chn

-- | Remove chains without liberties. Give player as an argument to solve atari. Return board.
updateBoard :: forall b c n. Game b c n => b -> PlayerN n -> b
updateBoard board player = foldl removeSurrounded board chains
  where chains = prepChains player $ accChains board
