{-# LANGUAGE FlexibleContexts #-}

module ServerState.Class ( MonadServerState (..)
                         , transact
                         , readGameSets
                         , writeGameSets
                         , actServer
                         ) where

import Control.Monad.Trans
import GHC.Conc

import Clients.Class
import GameSet.Internal

import Go.Game

class MonadClients m => MonadServerState m where

  gameSetsTVar :: m (TVar GameSets)

readGameSets :: (MonadServerState (t STM), MonadTrans t)
             => t STM GameSets
readGameSets = lift . readTVar =<< gameSetsTVar

writeGameSets :: (MonadServerState (t STM), MonadTrans t)
              => GameSets
              -> t STM ()
writeGameSets gs = lift . flip writeTVar gs =<< gameSetsTVar

-- | Update the 'GameState' in 'STM' and return the new 'GameState'.
actServer :: (MonadServerState (t STM), MonadTrans t)
          => ClientId
          -> ActionRep
          -> t STM (Either BadConfigServer GameSets)
actServer key action = do gameId <- maybe (Left BadConfigMismatch) Right . maybeGameId <$> getClient key
                          gameSets <- readGameSets
                          let eithGss = flip (updateGameSets (actGameSet key action)) gameSets =<< gameId
                          case eithGss of -- TODO: nicer without case statement?
                            Left ex -> return $ Left ex
                            Right gss -> do writeGameSets gss
                                            return $ Right gss -- TODO: don't return all GameSets, just one is better here?
