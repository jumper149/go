module ServerState.Class ( MonadServerState (..)
                         , transact
                         , readGameSets
                         , writeGameSets
                         ) where

import Control.Monad.Base
import GHC.Conc

import Clients.Class
import GameSet.Internal

import Go.Game

class MonadClients m => MonadServerState m where

  gameSetsTVar :: m (TVar GameSets)

readGameSets :: (MonadBase STM m, MonadServerState m)
             => m GameSets
readGameSets = liftBase . readTVar =<< gameSetsTVar

writeGameSets :: (MonadBase STM m, MonadServerState m)
              => GameSets
              -> m ()
writeGameSets gs = liftBase . flip writeTVar gs =<< gameSetsTVar
