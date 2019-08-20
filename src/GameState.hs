{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GameState ( StateTerm ( display
                             , startTerm
                             )
                 , GameState (..)
                 ) where

import Rules

-- | A player can execute the actions represented by this data type.
data Action c = Pass
              | Place c
  deriving (Eq)

-- | Apply action to board and number of passes.
act :: forall b c p. Game b c p => (b,Int) -> p -> Action c -> (b,Int)
act (board , passes) _ Pass = (board , passes + 1)
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

class Game b c p => StateTerm b c p where

  display :: b -> p -> String

  stepTerm :: GameState b p -> IO (b,p)
  stepTerm (GState board player oldBoard passes) =
    do putStr $ display board player
       action <- readIOSafe $ readAction board
       let (newBoard , newPasses) = act (board , passes) player action
           newPlayer = next player
       if newPasses < countPlayers player
       then if newBoard /= oldBoard
               then stepTerm (GState newBoard newPlayer board newPasses)
               else stepTerm (GState board player oldBoard passes)
       else stepTerm (GEnded newBoard newPlayer)
  stepTerm (GEnded board player) = putStr "end\n" >> return (board , player)

  startTerm :: IO (b,p)
  startTerm = stepTerm $ GState board player board 0
    where board = empty :: b
          player = minBound :: p

readIOSafe :: (String -> Maybe a) -> IO a
readIOSafe reader = reader <$> getLine >>= maybe (readIOSafe reader) return
