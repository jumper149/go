module Go.Game.End ( EndScreen (..)
                   , finalizeState
                   ) where

import GHC.Generics

import Go.Game.Game
import Go.Game.Player
import Go.Game.State

-- | Holds informaion for the endscreen.
data EndScreen b n = EndScreen { lastBoard :: b
                               , winner :: PlayerN n
                               , points :: [(PlayerN n,Int)]
                               , stonesOnBoard :: [(PlayerN n,Int)]
                               , turns :: Int
                               }
  deriving (Eq, Generic, Ord, Read, Show)

-- | Transform the state of a finished game to the endscreen.
finalizeState :: forall b c n. Game b c n => GameState b c n -> EndScreen b n
finalizeState gs = EndScreen { lastBoard = currentBoard gs
                             , winner = currentPlayer gs
                             , points = []
                             , stonesOnBoard = map (\ x -> (x , countStones (currentBoard gs) x)) [ minBound .. (maxBound :: PlayerN n) ]
                             , turns = countTurns gs
                             }

-- | Count the number of stones a player has on the board. Helper function for 'finalizeState'.
countStones :: forall b c n. Game b c n => b -> PlayerN n -> Int
countStones board player = length $ filter hasPlayerStone coords
  where hasPlayerStone coord = getStone board coord == Stone player
