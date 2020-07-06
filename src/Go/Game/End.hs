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
                               , points :: [(PlayerN n,Integer)]
                               , stonesOnBoard :: [(PlayerN n,Integer)]
                               , turns :: Integer
                               }
  deriving (Eq, Generic, Ord, Read, Show)

-- | Transform the state of a finished game to the endscreen.
finalizeState :: forall b. Game b => AssociatedGameState b -> EndScreen b (AssociatedPlayerCount b)
finalizeState gs = EndScreen { lastBoard = currentBoard gs
                             , winner = currentPlayer gs
                             , points = []
                             , stonesOnBoard = map (\ x -> (x , countStones (currentBoard gs) x)) [ minBound .. (maxBound :: AssociatedPlayer b) ]
                             , turns = countTurns gs
                             }

-- | Count the number of stones a player has on the board. Helper function for 'finalizeState'.
countStones :: forall b. Game b => b -> AssociatedPlayer b -> Integer
countStones board player = toEnum . length $ filter hasPlayerStone coords -- TODO: toEnum, makes Integer useless
  where hasPlayerStone coord = getStone board coord == Stone player
