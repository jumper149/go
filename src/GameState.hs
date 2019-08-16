{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GameState ( State (..)
                 , GameState (..)
                 , startGame
                 ) where

import Game

-- | A player can execute the actions represented by this data type.
data Action c = Pass
              | Place c
  deriving (Eq)

-- | Decide what and if a string represents an action.
readAction :: Board b c => b -> String -> Maybe (Action c)
readAction board str
  | str == "pass" = Just Pass
  | otherwise = Place <$> readCoordOnBoard board str

data GameState b c p = GState
                     | Ended

startGame :: forall b c p. Game b c p => IO (b,p)
startGame = runGame board player
  where board = empty :: b
        player = minBound :: p

-- | Make one step in the game and also start the next step.
runGame :: forall b c p. Game b c p => b -> p -> IO (b,p)
runGame board player = do -- putStr $ display board player
                          action <- readIOSafe $ readAction board
                          let newBoard = act board player action
                              newPlayer = next player
                          runGame newBoard newPlayer
  where readIOSafe :: (String -> Maybe (Action c)) -> IO (Action c)
        readIOSafe reader = reader <$> getLine >>= maybe (readIOSafe reader) return
        act :: b -> p -> Action c -> b
        act board player Pass = board
        act board player (Place coord) = updateBoard (putStone board coord (Stone player)) player

class Game b c p => State b c p where

  display :: b -> p -> String
