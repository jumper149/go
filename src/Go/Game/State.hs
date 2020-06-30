{-# LANGUAGE FlexibleContexts, RecordWildCards, StandaloneDeriving, UndecidableInstances #-}

module Go.Game.State ( GameState (..)
                     , Action (..)
                     , initState
                     ) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import GHC.TypeLits

import Go.Game.Game

-- | This data type contains the current board, the current player, the previous board and the
-- number of consecutive passes.
data GameState b = GState { currentBoard :: b
                          , currentPlayer :: AssociatedPlayer b
                          , lastAction :: AssociatedAction b
                          , previousBoards :: [b]
                          , consecutivePasses :: Integer
                          , countTurns :: Integer
                          }
  deriving Generic
deriving instance (KnownNat (AssociatedPlayerCount b), Eq b, Eq (AssociatedCoord b)) => Eq (GameState b)
deriving instance (KnownNat (AssociatedPlayerCount b), Ord b, Ord (AssociatedCoord b)) => Ord (GameState b)
deriving instance (KnownNat (AssociatedPlayerCount b), Read b, Read (AssociatedCoord b)) => Read (GameState b)
deriving instance (KnownNat (AssociatedPlayerCount b), Show b, Show (AssociatedCoord b)) => Show (GameState b)

instance (Game b, KnownNat (AssociatedPlayerCount b), Generic b, Generic (AssociatedCoord b), FromJSON b, FromJSON (AssociatedCoord b)) => FromJSON (GameState b)
instance (Game b, KnownNat (AssociatedPlayerCount b), Generic b, Generic (AssociatedCoord b), ToJSON b, ToJSON (AssociatedCoord b)) => ToJSON (GameState b)

initState :: Game b => GameState b
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
