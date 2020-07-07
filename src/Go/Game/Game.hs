module Go.Game.Game ( Game (..)
                    , AssociatedPlayer
                    , AssociatedStone
                    , updateBoard
                    , GameCoord (..)
                    , Stone (..)
                    ) where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Set as S
import GHC.Generics
import GHC.TypeLits

import Go.Game.Player

-- | The states of a spot for a stone are represented by this data type.
data Stone p = Free
             | Stone p
  deriving (Eq, Ord, Generic, Read, Show)

instance (Generic p, FromJSON p) => FromJSON (Stone p)
instance (Generic p, ToJSON p) => ToJSON (Stone p)

-- | Stones placed on coordinates can form chains which are represented by this data type.
data Chain p c = Chain (Stone p) (S.Set c)
  deriving (Eq, Ord, Generic, Read, Show)

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

-- | A class representing coordinates. All coordinates should be enumerable with
-- '[minBound..maxBound]', so 'Bounded' and 'Enum' should be declared carefully. The ordering for
-- 'Ord' can be arbitrary.
class (Bounded c, Enum c, Eq c, Ord c) => GameCoord c where

  -- | Return a list of all coordinates covering the board.
  coords :: [c]
  coords = [ minBound .. maxBound ]

  -- | Return a list of all adjacent coordinates.
  libertyCoords :: c -> [c]

class (Eq b, GameCoord (AssociatedCoord b), KnownNat (AssociatedPlayerCount b)) => Game b where

  type AssociatedCoord b

  type AssociatedPlayerCount b :: Nat

  -- | Return an empty board.
  empty :: b

  -- | Returns the stone.
  getStone :: b -> AssociatedCoord b -> Stone (PlayerN (AssociatedPlayerCount b))

  -- | Places a stone on a coordinate and returns the board.
  putStone :: b -> AssociatedCoord b -> Stone (PlayerN (AssociatedPlayerCount b)) -> b

type AssociatedPlayer b = PlayerN (AssociatedPlayerCount b)

type AssociatedStone b = Stone (AssociatedPlayer b)

accChain :: forall b. Game b => b -> AssociatedStone b -> AssociatedCoord b -> S.Set (AssociatedCoord b) -> S.Set (AssociatedCoord b)
accChain board stone coord acc = foldr (accChain board stone) newAcc neededSames
  where newAcc = acc `S.union` sames
        neededSames = sames `S.difference` acc
        sames = S.filter ((== stone) . getStone board) libs
        libs = S.fromList $ libertyCoords coord

chain :: forall b. Game b => b -> AssociatedCoord b -> Chain (AssociatedPlayer b) (AssociatedCoord b)
chain board coord = Chain stone crds
  where stone = getStone board coord
        crds = accChain board stone coord acc
        acc = S.singleton coord

accChains :: forall b. Game b => b -> S.Set (Chain (AssociatedPlayer b) (AssociatedCoord b))
accChains board = foldr addCoord S.empty coords -- TODO: only occurence of 'coords', maybe remove that method completely?
  where addCoord crd chns = if any (partOfChain crd) chns
                               then chns
                               else chain board crd `S.insert` chns

hasLiberty :: forall b. Game b => b -> Chain (AssociatedPlayer b) (AssociatedCoord b) -> Bool
hasLiberty board (Chain _ crds) = or bools
  where bools = S.map ((== Free) . (getStone board :: (AssociatedCoord b -> AssociatedStone b))) libs
        libs = S.unions $ S.map (S.fromList . libertyCoords) crds

removeChain :: forall b. Game b => b -> Chain (AssociatedPlayer b) (AssociatedCoord b) -> b
removeChain board (Chain _ crds) = foldl putFree board crds
  where putFree brd crd = putStone brd crd Free

removeSurrounded :: forall b. Game b => b -> Chain (AssociatedPlayer b) (AssociatedCoord b) -> b
removeSurrounded brd chn = if hasLiberty brd chn
                              then brd
                              else removeChain brd chn

-- | Remove chains without liberties. Give player as an argument to solve atari. Return board.
updateBoard :: forall b. Game b => b -> AssociatedPlayer b -> b
updateBoard board player = foldl removeSurrounded board chains
  where chains = prepChains player $ accChains board
