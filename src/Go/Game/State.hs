{-# LANGUAGE FlexibleContexts, RecordWildCards #-}

module Go.Game.State ( GameState (..)
                     , Action (..)
                     , initState
                     ) where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import GHC.TypeLits

import Go.Game.Config
import Go.Game.Game
import Go.Game.Player

-- | This data type contains the current board, the current player, the previous board and the
-- number of consecutive passes.
data GameState b = GState { currentBoard :: b
                          , currentPlayer :: AssociatedPlayer b
                          , lastAction :: AssociatedAction b
                          , previousBoards :: [b]
                          , consecutivePasses :: Integer
                          , countTurns :: Integer
                          }
  deriving (Eq, Generic, Ord, Read, Show)

instance (Game b, KnownNat (AssociatedPlayerCount b), Generic b, Generic (AssociatedCoord b), FromJSON b, FromJSON (AssociatedCoord b)) => FromJSON (GameState b)
instance (Game b, KnownNat (AssociatedPlayerCount b), Generic b, Generic (AssociatedCoord b), ToJSON b, ToJSON (AssociatedCoord b)) => ToJSON (GameState b c n)

initState :: (Game b c n, Monad m, MonadError Malconfig m, MonadReader Config m)
          => m (GameState b c n)
initState = return $ GState {..}
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
