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

-- | Set up the game for the function executing each turn.
start :: forall b c p m. (Game b c p, Monad m) => (GameState b p -> m (b,p)) -> m (b,p)
start stepper = stepper (GState board player board 0)
  where board = empty :: b
        player = minBound :: p

-- | Execute one turn. This function recursively calls itself until the game is over.
step :: forall b c p m. (Game b c p, Monad m) => (GameState b p -> m (b,p)) -> GameState b p -> Action c -> m (b,p)
step stepper (GState board player oldBoard passes) action =
  if newPasses < countPlayers player
  then if newBoard /= oldBoard
          then stepper (GState newBoard newPlayer board newPasses)
          else stepper (GState board player oldBoard passes)
  else return (newBoard , newPlayer)
  where (newBoard , newPasses) = act (board , passes) player action
        newPlayer = next player

-- | This data type contains the current board, the current player, the previous board and the
-- number of consecutive passes.
data GameState b p = GState b p b Int

class Game b c p => StateTerm b c p where

  display :: b -> p -> String

  startTerm :: IO (b,p)
  startTerm = start stepTerm :: IO (b,p)

  stepTerm :: GameState b p -> IO (b,p)
  stepTerm (GState board player oldBoard passes) =
    do putStr $ display board player
       action <- readIOSafe $ readAction board
       endState <- step stepTerm (GState board player oldBoard passes) action
       endTerm endState

  endTerm :: (b,p) -> IO (b,p)
  endTerm (board , player) = putStr (display board player) >> return (board , player)

-- | Decide what and if a string represents an action.
readAction :: forall b c. Board b c => b -> String -> Maybe (Action c)
readAction board str
  | str == "pass" = Just Pass
  | otherwise = Place <$> readCoordOnBoard board str

-- | Read strings from IO, until one is accepted by the reader function.
readIOSafe :: (String -> Maybe a) -> IO a
readIOSafe reader = reader <$> getLine >>= maybe (readIOSafe reader) return
