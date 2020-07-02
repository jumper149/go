{-# LANGUAGE RecordWildCards #-}

module Go.Game ( GameStateRep (..)
               , ActionRep (..)
               , actRep
               , initStateRep
               ) where

import Control.Monad.Except
import GHC.Generics

import qualified Go.Board.Default as D
import Go.Config
import Go.Game.Act
import Go.Game.State

data BoardRep = BoardD_9_2 (D.Board 9 2)
              | BoardD_13_2 (D.Board 13 2)
  deriving Generic

data ActionRep = ActionD_9_2 (AssociatedAction (D.Board 9 2))
               | ActionD_13_2 (AssociatedAction (D.Board 13 2))
  deriving Generic

data GameStateRep = GameStateD_9_2 (AssociatedGameState (D.Board 9 2))
                  | GameStateD_13_2 (AssociatedGameState (D.Board 13 2))
  deriving Generic

actRep :: MonadConfig m => ActionRep -> GameStateRep -> m GameStateRep
actRep action gamestate = do Config {..} <- config
                             case (action , gamestate) of
                               (ActionD_9_2 a , GameStateD_9_2 gs) -> GameStateD_9_2 <$> (embedRuleViolation $ act ruleset a gs)
                               (ActionD_13_2 a , GameStateD_13_2 gs) -> GameStateD_13_2 <$> (embedRuleViolation $ act ruleset a gs)
                               _ -> throwError BadConfigActionMismatch

initStateRep :: MonadConfig m => m GameStateRep
initStateRep = do Config {..} <- config
                  case board of
                    Default -> case size of
                                 9 -> return $ GameStateD_9_2 initState
                                 13 -> return $ GameStateD_13_2 initState
                                 _ -> throwError BadConfigSize
                    _ -> throwError BadConfigBoard
