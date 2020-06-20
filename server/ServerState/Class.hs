{-# LANGUAGE FlexibleContexts, FunctionalDependencies #-}

module ServerState.Class ( MonadServerState (..)
                         , transact
                         , readServerGameState
                         , writeServerGameState
                         , serverUpdateGameState
                         ) where

import Control.Monad.Base
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Trans.Control.Identity
import GHC.Conc
import GHC.Generics

import Clients.Class

import Go.Game.Act
import Go.Game.Config
import Go.Game.Game
import Go.Game.State

class MonadClients n m => MonadServerState b c n m | m -> b c n where

  gameStateTVar :: m (TVar (GameState b c n))

  gameConfig :: m Config

readServerGameState :: (MonadServerState b c n (t STM), MonadTrans t)
                    => t STM (GameState b c n)
readServerGameState = lift . readTVar =<< gameStateTVar

writeServerGameState :: (MonadServerState b c n (t STM), MonadTrans t)
                     => GameState b c n
                     -> t STM ()
writeServerGameState gs = lift . flip writeTVar gs =<< gameStateTVar

serverGameConfig :: MonadServerState b c n m => m Config
serverGameConfig = gameConfig

-- | Update the 'GameState' in 'STM' and return the new 'GameState'.
serverUpdateGameState :: (Game b c n, MonadServerState b c n (t STM), MonadTrans t)
                      => ClientId
                      -> Action c
                      -> t STM (Either String (GameState b c n))
serverUpdateGameState key action = do clientIsCurrent <- serverIsCurrentPlayer key
                                      if clientIsCurrent
                                         then do eithGs <- act <$> (ruleset <$> gameConfig) <*> pure action <*> readServerGameState
                                                 case eithGs of
                                                   Right gs -> do writeServerGameState gs
                                                                  return $ Right gs
                                                   Left ex  -> return $ Left $ show ex
                                         else return $ Left "it's not your turn"

-- | Check if given 'ClientId' is the 'currentPlayer'. Helper function for 'serverUpdateGameState'.
serverIsCurrentPlayer :: (MonadServerState b c n (t STM), MonadTrans t)
                      => ClientId
                      -> t STM Bool
serverIsCurrentPlayer key = do client <- getClient key
                               gs <- readServerGameState
                               case maybePlayer client of
                                 Nothing -> return False
                                 Just p -> return $ p == currentPlayer gs
