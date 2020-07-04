{-# LANGUAGE FlexibleContexts #-}

module GameSet.Class ( MonadGameSet (..)
                     , GameSet
                     , gameState
                     , GameId
                     , BadConfigServer (..)
                     , transact
                     , actGame -- TODO: rename?
                     , updatePlayer
                     ) where

import Control.Monad.Base
import Control.Monad.Trans
import GHC.Conc

import Clients.Class
import GameSet.Internal

import Go.Game
import Go.Player

class MonadBase STM m => MonadGameSet m where

  readPlayer :: m Client

  readPlayers :: m [Client]

  readGameSet :: m GameSet

  writeGameSet :: GameSet -> m ()

-- | Update the 'GameState' in 'STM' and return the new 'GameState'.
actGame :: MonadGameSet m
        => ActionRep
        -> m (Either BadConfigServer GameSet)
actGame action = do player <- identification <$> readPlayer
                    eithGs <- actGameSet player action <$> readGameSet
                    case eithGs of -- TODO: nicer without case statement?
                      Left ex -> return $ Left ex
                      Right gs -> do writeGameSet gs
                                     return $ Right gs

updatePlayer :: MonadGameSet m
             => Maybe PlayerRep
             -> m (Either BadConfigServer (Maybe PlayerRep))
updatePlayer p = do eithGs <- addPlayerTo <$> (identification <$> readPlayer) <*> pure p <*> readGameSet
                    case eithGs of
                      Left ex -> return $ Left ex
                      Right gs -> do writeGameSet gs
                                     return $ Right p
