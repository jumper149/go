module GameState ( start
                 , GameState (..)
                 , EndScreen (..)
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
data GameState b p = GState { currBoard :: b
                            , currPlayer :: p
                            , prevBoard :: b
                            , countPasses :: Int
                            , countRounds :: Int
                            , messageOnPrev :: String
                            }

data Status = StatOK
            | StatEnd

-- Safely apply an action to the state of a game.
actOnGame :: forall b c p. Game b c p => GameState b p -> Action c -> (GameState b p , Status)
actOnGame state action =
  if newPasses < countPlayers (currPlayer state)
  then if wasFree && newBoard /= prevBoard state
          then (newState , StatOK)
          else (state { messageOnPrev = "redo" }, StatOK)
  else (newState { messageOnPrev = "ended" } , StatEnd)
  where wasFree = case action of
                    Place coord -> getStone (currBoard state) coord == Free
                    Pass -> True
        (newBoard , newPasses) = act (currBoard state , countPasses state) (currPlayer state) action
        newState = state { currBoard = newBoard
                         , currPlayer = next (currPlayer state)
                         , prevBoard = currBoard state
                         , countPasses = newPasses
                         , messageOnPrev = ""
                         }

-- | Set up the game for the function executing each turn.
start :: forall b c p m. (Game b c p, Monad m) => (String -> GameState b p -> m (Action c)) -> (EndScreen b p -> m (b,p)) -> m (b,p)
start stepper ender = step stepper startState StatOK >>= end >>= ender
  where startState = GState { currBoard = empty :: b
                            , currPlayer = minBound :: p
                            , prevBoard = empty :: b
                            , countPasses = 0
                            , countRounds = 0
                            , messageOnPrev = ""
                            }

-- | Execute one turn by calling a function that reads an action.
step :: forall b c p m. (Game b c p, Monad m) => (String -> GameState b p -> m (Action c)) -> GameState b p -> Status -> m (GameState b p)
step stepper state StatOK =
  do action <- stepper (messageOnPrev state) state
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
end state = return $ EndScreen { lastBoard = currBoard state
                               , winner = currPlayer state
                               , points = []
                               , stonesOnBoard = map (\ x -> (x , countStones (currBoard state) x)) ([ minBound .. maxBound ] :: [p])
                               , turns = 0
                               }

-- | Return the next player.
next :: forall p. Player p => p -> p
next player = if player == maxBound
              then minBound
              else succ player

-- | Count the number of players.
countPlayers :: forall p. Player p => p -> Int
countPlayers _ = length ([ minBound .. maxBound ] :: [p])

-- | Count the number of stones a player has on the board.
countStones :: forall b c p. Game b c p => b -> p -> Int
countStones board player = length $ filter hasPlayerStone $ coords board
  where hasPlayerStone :: c -> Bool
        hasPlayerStone coord = getStone board coord == Stone player
