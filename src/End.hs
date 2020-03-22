module End ( EndScreen (..)
           , finalizeState
           ) where

import Game
import State

-- | Holds informaion for the endscreen.
data EndScreen b p = EndScreen { lastBoard :: b
                               , winner :: p
                               , points :: [(p,Int)]
                               , stonesOnBoard :: [(p,Int)]
                               , turns :: Int
                               }

-- | Transform the state of a finished game to the endscreen.
finalizeState :: forall b c p. Game b c p => GameState b c p -> EndScreen b p
finalizeState gs = EndScreen { lastBoard = currentBoard gs
                             , winner = currentPlayer gs
                             , points = []
                             , stonesOnBoard = map (\ x -> (x , countStones (currentBoard gs) x)) ([ minBound .. maxBound ] :: [p])
                             , turns = countTurns gs
                             }

-- | Count the number of stones a player has on the board. Helper function for 'finalizeState'.
countStones :: forall b c p. Game b c p => b -> p -> Int
countStones board player = length $ filter hasPlayerStone $ coords board
  where hasPlayerStone coord = getStone board coord == Stone player
