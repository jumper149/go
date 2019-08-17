{-# LANGUAGE MultiParamTypeClasses #-}
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

act :: forall b c p. Game b c p => b -> p -> Action c -> b
act board player Pass = board
act board player (Place coord) = updateBoard (putStone board coord (Stone player)) player

-- | Decide what and if a string represents an action.
readAction :: forall b c. Board b c => b -> String -> Maybe (Action c)
readAction board str
  | str == "pass" = Just Pass
  | otherwise = Place <$> readCoordOnBoard board str

-- board player oldBoard numberOfPasses
data GameState b p = GStarted b p
                   | GState b p b Int
                   | GEnded b p

class Game b c p => State b c p where

  display :: b -> String

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

stepGame :: forall b c p. State b c p => GameState b p -> IO (GameState b p)
stepGame (GStarted board player) = return (GStarted board player)

endGame :: forall b c p. State b c p => GameState b p -> IO (b,p)
endGame (GEnded board player) = putStr "end" >> return (board , player)

readIOSafe :: (String -> Maybe (Action c)) -> IO (Action c)
readIOSafe reader = reader <$> getLine >>= maybe (readIOSafe reader) return
