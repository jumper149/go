{-# LANGUAGE ScopedTypeVariables #-}

module GameState ( GameState (..)
                 , EndScreen (..)
                 , start
                 , step
                 , end
                 , Action (..)
                 ) where

import Rules

-- | A player can execute the actions represented by this data type.
data Action c = Pass
              | Place c
  deriving (Eq)

-- | Apply action to board and handle number of passes. Doesn't check for sanity.
act :: forall b c p. Game b c p => (b,Int) -> p -> Action c -> (b,Int)
act (board , passes) _ Pass = (board , passes + 1)
act (board , _) player (Place coord) = (newBoard , 0)
  where newBoard = updateBoard (putStone board coord (Stone player)) player

-- | Set up the game for the function executing each turn.
start :: forall b c p m. (Game b c p, Monad m) => (GameState b p -> m (GameState b p)) -> (EndScreen b p -> m (b,p)) -> m (b,p)
start stepper ender = stepper (GState board player board 0) >>= end >>= ender
  where board = empty :: b
        player = minBound :: p

-- | Execute one turn. This function recursively calls itself until the game is over.
step :: forall b c p m. (Game b c p, Monad m) => (GameState b p -> m (GameState b p)) -> GameState b p -> Action c -> m (GameState b p)
step stepper (GState board player oldBoard passes) action =
  if newPasses < countPlayers player
  then if wasFree && newBoard /= oldBoard
          then stepper (GState newBoard newPlayer board newPasses)
          else stepper (GState board player oldBoard passes)
  else return (GState newBoard newPlayer undefined newPasses)
  where wasFree = case action of
                    Place coord -> getStone board coord == Free
                    Pass -> True
        (newBoard , newPasses) = act (board , passes) player action
        newPlayer = next player

-- | This data type contains the current board, the current player, the previous board and the
-- number of consecutive passes.
data GameState b p = GState b p b Int

data EndScreen b p = EndScreen { board :: b
                               , points :: [(p,Int)]
                               , stonesOnBoard :: [(p,Int)]
                               , turns :: Int
                               }

end :: forall b c p m. (Game b c p, Monad m) => (GameState b p) -> m (EndScreen b p)
end (GState brd _ _ _) = return $ EndScreen { board = brd
                                            , points = []
                                            , stonesOnBoard = []
                                            , turns = 0
                                            }
