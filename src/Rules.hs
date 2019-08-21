{-# LANGUAGE FunctionalDependencies, ScopedTypeVariables #-}

module Rules ( Game ( getStone
                    , putStone
                    , updateBoard
                    )
             , Board (..)
             , Player (..)
             , next
             , countPlayers
             , Stone (..)
             , showStone
             ) where

import qualified Data.Set as S
import Data.List (sortOn)

-- | The states of a spot for a stone are represented by this data type.
data Stone p = Free
             | Stone p
  deriving (Eq, Ord)

-- | Show a stone preferably as a single character string.
showStone :: forall p. Player p => Stone p -> String
showStone Free = " "
showStone (Stone p) = [ char p ]

-- | Stones placed on coordinates can form chains which are represented by this data type.
data Chain p c = Chain (Stone p) (S.Set c)
  deriving (Eq, Ord)

class (Eq p, Enum p, Bounded p, Ord p) => Player p where

  -- | Represent a player with a preferably unique character.
  char :: p -> Char

-- | Return the next player.
next :: forall p. Player p => p -> p
next player = if player == maxBound
              then minBound
              else succ player

-- | Count the number of players.
countPlayers :: forall p. Player p => p -> Int
countPlayers _ = length ([ minBound .. maxBound ] :: [p])

class (Eq b, Eq c, Ord c) => Board b c | b -> c where

  -- | Return an empty board.
  empty :: b

  -- | Return a list of all coords covering the board.
  coords :: b -> [c]

  -- | Return a list of all adjacent coordinates.
  libertyCoords :: b -> c -> [c]

  -- | Decide if a string represents a coordinate and read it.
  readCoordOnBoard :: b -> String -> Maybe c

class (Board b c, Player p) => Game b c p | b -> c p where

  -- | Returns the stone.
  getStone :: b -> c -> Stone p

  -- | Places a stone on a coordinate and returns the board.
  putStone :: b -> c -> Stone p -> b

  accChain :: b -> Stone p -> c -> S.Set c -> S.Set c
  accChain board stone coord acc = sames `S.union` recChain
    where sames = S.filter ((== stone) . getStone board) libs :: S.Set c
          libs = S.fromList $ libertyCoords board coord :: S.Set c
          recChain = S.unions $ S.map rec nextCoords :: S.Set c
          nextCoords = sames S.\\ acc :: S.Set c
          newAcc = acc `S.union` sames :: S.Set c

          rec :: c -> S.Set c
          rec x = accChain board stone x newAcc

  chain :: b -> c -> Chain p c
  chain board coord = Chain stone crds
    where stone = getStone board coord :: Stone p
          crds = singletonCoord `S.union` accChain board stone coord singletonCoord :: S.Set c
          singletonCoord = S.singleton coord :: S.Set c

  chains :: b -> p -> [Chain p c]
  chains board player = appendPrevs sorted []
    where sorted = sortOn (\ (Chain (Stone x) _) -> x) $ S.toList withoutFrees
          withoutFrees = S.filter (\ (Chain stone _) -> stone /= Free) allChains
          allChains = S.map (chain board) $ S.fromList (coords board)

          appendPrevs :: [Chain p c] -> [Chain p c] -> [Chain p c]
          appendPrevs [] acc = acc
          appendPrevs (Chain (Stone x) y : cs) acc
            | x < player = appendPrevs cs (acc ++ [ Chain (Stone x) y ])
            | otherwise = (Chain (Stone x) y : cs) ++ acc
          appendPrevs (Chain Free _ : _) _ = undefined

  hasLiberty :: b -> Chain p c -> Bool
  hasLiberty board (Chain _ crds) = S.foldr (||) False bools
    where bools = S.map ((== (Free :: Stone p)) . (getStone board :: c -> Stone p)) libs
          libs = S.unions $ S.map (S.fromList . libertyCoords board) crds

  removeChain :: b -> Chain p c -> b
  removeChain board (Chain _ crds) = S.foldr putFree board crds
    where putFree :: c -> b -> b
          putFree crd brd = putStone brd crd (Free :: Stone p)

  -- | Remove chains without liberties. Give player as an argument to solve atari. Return board.
  updateBoard :: b -> p -> b
  updateBoard board player = foldl removeNoLiberty board chs
    where chs = chains board player

          removeNoLiberty :: b -> Chain p c -> b
          removeNoLiberty brd chn = if hasLiberty brd chn
                                    then brd
                                    else removeChain brd chn

--countStones :: forall b c p. Game b c p => b -> p -> Int
--countStones board player = length $ filter hasPlayerStone $ coords board
--  where hasPlayerStone :: c -> Bool
--        hasPlayerStone coord = getStone board coord == Stone player
