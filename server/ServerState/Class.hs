module ServerState.Class ( module Clients.Class
                         , MonadServerState (..)
                         , readGameSets
                         , writeGameSets
                         , addGameSet
                         , getGameSet
                         , gameSetList
                         , tryConfig
                         ) where

import Control.Monad.Base
import GHC.Conc

import Clients.Class
import GameSet.Class
import ServerState.Internal

import Go.Config
import Go.Representation

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
           -> m (Either BadConfigServer [GameId])
addGameSet c = do gss <- readGameSets
                  case newGameSetFor c gss of
                    Left ex -> return $ Left ex
                    Right gs -> do writeGameSets $ addGameSetTo gs gss
                                   Right <$> gameSetList

getGameSet :: (MonadBase STM m, MonadServerState m)
           => GameId
           -> m (Maybe GameSet)
getGameSet k = getGameSetFrom k <$> readGameSets

gameSetList :: (MonadBase STM m, MonadServerState m)
            => m [GameId]
gameSetList = fmap fst . gameSetListFrom <$> readGameSets

tryConfig :: Config -> Either BadConfigServer Config
tryConfig c =  case newGameSetFor c mempty of
                 Left ex -> Left ex
                 Right _ -> Right c
