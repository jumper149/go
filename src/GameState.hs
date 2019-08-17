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

-- | Apply action to board and number of passes.
act :: forall b c p. Game b c p => (b,Int) -> p -> Action c -> (b,Int)
act (board , passes) player Pass = (board , passes + 1)
act (board , _) player (Place coord) = (newBoard , 0)
  where newBoard = updateBoard (putStone board coord (Stone player)) player

-- | Decide what and if a string represents an action.
readAction :: forall b c. Board b c => b -> String -> Maybe (Action c)
readAction board str
  | str == "pass" = Just Pass
  | otherwise = Place <$> readCoordOnBoard board str

-- board player oldBoard numberOfPasses
data GameState b p = GState b p b Int
                   | GEnded b p

class Game b c p => State b c p where

  display :: b -> String

startGame :: forall b c p. State b c p => IO (b,p)
startGame = stepGame $ GState board player board 0
  where board = empty :: b
        player = minBound :: p

stepGame :: forall b c p. State b c p => GameState b p -> IO (b,p)
stepGame (GState board player oldBoard passes) =
  do putStr $ display board
     action <- readIOSafe $ readAction board
     let (newBoard , newPasses) = act (board , passes) player action
         newPlayer = next player
     if newPasses < countPlayers player
     then if newBoard /= oldBoard
             then stepGame (GState newBoard newPlayer board newPasses)
             else stepGame (GState board player oldBoard passes)
     else endGame (GEnded newBoard newPlayer)

endGame :: forall b c p. State b c p => GameState b p -> IO (b,p)
endGame (GEnded board player) = putStr "end" >> return (board , player)

readIOSafe :: (String -> Maybe (Action c)) -> IO (Action c)
readIOSafe reader = reader <$> getLine >>= maybe (readIOSafe reader) return
