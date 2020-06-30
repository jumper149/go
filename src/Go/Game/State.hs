{-# LANGUAGE FlexibleContexts, RecordWildCards #-}

module Go.Game.State ( GameState (..)
                     , AssociatedGameState
                     , Action (..)
                     , AssociatedAction
                     , initState
                     ) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import GHC.TypeLits

import Go.Game.Player
import Go.Game.Game

-- | This data type contains the current board, the current player, the previous board and the
-- number of consecutive passes.
data GameState b c n = GState { currentBoard :: b
                              , currentPlayer :: PlayerN n
                              , lastAction :: Action c
                              , previousBoards :: [b]
                              , consecutivePasses :: Integer
                              , countTurns :: Integer
                              }
  deriving (Eq, Generic, Ord, Read, Show)

instance (KnownNat n, Generic b, Generic c, FromJSON b, FromJSON c) => FromJSON (GameState b c n)
instance (KnownNat n, Generic b, Generic c, ToJSON b, ToJSON c) => ToJSON (GameState b c n)

type AssociatedGameState b = GameState b (AssociatedCoord b) (AssociatedPlayerCount b)

initState :: Game b => AssociatedGameState b
initState = GState {..}
  where currentBoard = empty
        currentPlayer = minBound
        lastAction = Pass
        previousBoards = [ empty ]
        consecutivePasses = 0
        countTurns = 0

-- | A player can execute the actions represented by this data type.
data Action c = Pass
              | Place c
  deriving (Eq, Generic, Ord, Read, Show)

type AssociatedAction b = Action (AssociatedCoord b)

instance (Generic c, FromJSON c) => FromJSON (Action c)
instance (Generic c, ToJSON c) => ToJSON (Action c)
