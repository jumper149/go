module ServerState.Class ( module Clients.Class
                         , MonadServerState (..)
                         , transact -- TODO: remove from here
                         , readGameSets
                         , writeGameSets
                         , addGameSet
                         , getGameSet
                         ) where

import Control.Monad.Base
import GHC.Conc

import Clients.Class
import GameSet.Class
import ServerState.Internal

import Go.Config
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

addGameSet :: (MonadBase STM m, MonadServerState m)
           => Config
           -> m (Either BadConfigServer ())
addGameSet c = do gss <- readGameSets
                  case newGameSetFor c gss of
                    Left ex -> return $ Left ex
                    Right gs -> do writeGameSets $ addGameSetTo gs gss
                                   return $ Right ()

getGameSet :: (MonadBase STM m, MonadServerState m)
           => GameId
           -> m GameSet
getGameSet k = getGameSetFrom k <$> readGameSets
