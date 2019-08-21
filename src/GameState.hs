{-# LANGUAGE ScopedTypeVariables #-}

module GameState ( GameState (..)
                 , EndScreen (..)
                 , start
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

-- | This data type contains the current board, the current player, the previous board and the
-- number of consecutive passes.
data GameState b p = GState b p b Int

data Status = StatOK
            | StatMsg String
            | StatEnd

-- Safely apply an action to the state of a game.
actOnGame :: forall b c p. Game b c p => GameState b p -> Action c -> (GameState b p , Status)
actOnGame (GState board player oldBoard passes) action =
  if newPasses < countPlayers player
  then if wasFree && newBoard /= oldBoard
          then (GState newBoard newPlayer board newPasses , StatOK)
          else (GState board player oldBoard passes , StatMsg "redo")
  else (GState newBoard newPlayer undefined newPasses , StatEnd)
  where wasFree = case action of
                    Place coord -> getStone board coord == Free
                    Pass -> True
        (newBoard , newPasses) = act (board , passes) player action
        newPlayer = next player

-- | Set up the game for the function executing each turn.
start :: forall b c p m. (Game b c p, Monad m) => (String -> GameState b p -> m (Action c)) -> (EndScreen b p -> m (b,p)) -> m (b,p)
start stepper ender = step stepper startState StatOK >>= end >>= ender
  where startState = GState board player board 0
        board = empty :: b
        player = minBound :: p

-- | Execute one turn by calling a function that reads an action.
step :: forall b c p m. (Game b c p, Monad m) => (String -> GameState b p -> m (Action c)) -> GameState b p -> Status -> m (GameState b p)
step stepper state StatOK =
  do action <- stepper "" state
     let (newState , newStatus) = actOnGame state action
     step stepper newState newStatus
step stepper state (StatMsg message) =
  do action <- stepper message state
     let (newState , newStatus) = actOnGame state action
     step stepper newState newStatus
step _ state StatEnd = return state

-- | Holds informaion for the endscreen.
data EndScreen b p = EndScreen { lastBoard :: b
                               , winner :: p
                               , points :: [(p,Int)]
                               , stonesOnBoard :: [(p,Int)]
                               , turns :: Int
                               }

-- | End the game by returning an the information for an endscreen.
end :: forall b c p m. (Game b c p, Monad m) => GameState b p -> m (EndScreen b p)
end (GState brd plr _ _) = return $ EndScreen { lastBoard = brd
                                              , winner = plr
                                              , points = []
                                              , stonesOnBoard = []
                                              , turns = 0
                                              }
