{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Board ( Player ( char
                      , showStone
                      )
             , Stone (..)
             , Board ( empty
                     , coords
                     , unsafeLibertyCoords
                     , readCoordOnBoard
                     )
             , Game ( getStone
                    , putStone
                    , startGame
                    , showGame
                    )
             ) where

import qualified Data.Set as S
import Data.List (sortOn)

data Action c = Pass
              | Place c
  deriving (Eq)

data Stone p = Free
             | Stone p
  deriving (Eq, Ord)

data Chain p c = Chain (Stone p) (S.Set c)
  deriving (Eq, Ord)

class (Eq p, Enum p, Bounded p, Ord p) => Player p where
  char :: p -> Char
  next :: p -> p
  next player = if player == maxBound
                then minBound
                else succ player
  showStone :: Player p => Stone p -> String
  showStone Free = " "
  showStone (Stone p) = [ char p ]

class (Eq b, Eq c, Ord c) => Board b c | b -> c where
  empty :: b
  coords :: b -> [c]
  unsafeLibertyCoords :: b -> c -> [c]
  libertyCoords :: b -> c -> S.Set c
  libertyCoords board coord = S.fromList $ filter ((flip elem) (coords board)) $ unsafeLibertyCoords board coord
  readCoordOnBoard :: b -> String -> Maybe c
  readAction :: b -> String -> Maybe (Action c)
  readAction board str
    | str == "pass" = Just Pass
    | otherwise = fmap Place $ readCoordOnBoard board str

class (Board b c, Player p) => Game b c p | b -> c where
  getStone :: b -> c -> Stone p
  putStone :: b -> c -> Stone p -> b

  accChain :: b -> Stone p -> c -> S.Set c -> S.Set c
  accChain board stone coord acc = sames `S.union` recChain
    where sames = S.filter ((== stone) . getStone board) $ libertyCoords board coord :: S.Set c
          newAcc = acc `S.union` sames :: S.Set c
          nextCoords = sames S.\\ acc :: S.Set c
          rec :: c -> S.Set c
          rec x = accChain board stone x newAcc
          recChain = S.unions $ S.map rec nextCoords :: S.Set c

  chain :: b -> c -> Chain p c
  chain board coord = Chain stone coords
    where coords = singletonCoord `S.union` accChain board stone coord singletonCoord :: S.Set c
          singletonCoord = S.singleton coord
          stone = getStone board coord :: Stone p

  chains :: b -> S.Set (Chain p c)
  chains board = S.map (chain board) $ S.fromList (coords board)

  sortedChains :: b -> p -> [Chain p c]
  sortedChains board player = appendPrevs sorted []
    where withoutFrees = S.filter (\ (Chain stone _) -> stone /= Free) $ chains board
          sorted = sortOn (\ (Chain (Stone x) _) -> x) $ S.toList $ withoutFrees
          appendPrevs :: [Chain p c] -> [Chain p c] -> [Chain p c]
          appendPrevs [] acc = acc
          appendPrevs ((Chain (Stone x) y):cs) acc
            | x < player = appendPrevs cs (acc ++ [ (Chain (Stone x) y) ])
            | otherwise = ((Chain (Stone x) y) : cs) ++ acc

  hasLiberty :: b -> Chain p c -> Bool
  hasLiberty board (Chain stone coords) = S.foldr (||) False bools
    where bools = S.map (((== (Free :: Stone p)) . (getStone board :: c -> Stone p))) libs
          libs = S.unions $ S.map (libertyCoords board) coords

  removeChain :: b -> Chain p c -> b
  removeChain board (Chain _ coords) = S.foldr putFree board coords
    where putFree :: c -> b -> b
          putFree coord board = putStone board coord (Free :: Stone p)

  updateBoard :: b -> p -> b
  updateBoard board player = foldl removeNoLiberty board srtedChs
    where srtedChs = sortedChains board player
          removeNoLiberty :: b -> Chain p c -> b
          removeNoLiberty board chain = if hasLiberty board chain
                                        then board
                                        else removeChain board chain

  startGame :: IO (b,p)
  startGame = runGame board player
    where board = empty :: b
          player = minBound :: p

  showGame :: b -> p -> String

  runGame :: b -> p -> IO (b,p)
  runGame board player = do putStr $ showGame board player
                            action <- readIOSafe $ readAction board
                            let newBoard = act board player action
                                newPlayer = next player
                            runGame newBoard newPlayer
    where readIOSafe :: (String -> Maybe (Action c)) -> IO (Action c)
          readIOSafe reader = reader <$> readLn >>= maybe (readIOSafe reader) return
          act :: b -> p -> Action c -> b
          act board player Pass = board
          act board player (Place coord) = updateBoard (putStone board coord (Stone player)) player
