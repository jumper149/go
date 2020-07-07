{-# LANGUAGE RecordWildCards #-}

module Go.Game.Act ( RuleViolation (..)
                   , act
                   , initState
                   ) where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Proxy
import GHC.TypeLits

import Go.Game.Game
import Go.Game.Player
import Go.Game.Rules
import Go.Game.State

-- | Apply action to 'GameState' and handle all fields in 'GameState'. Doesn't check rules.
act :: forall b. Game b => Rules -> AssociatedAction b -> AssociatedGameState b -> Either RuleViolation (AssociatedGameState b)
act rls action gs = checkRules rls $
  GState { currentBoard = case action of
                            Pass -> currentBoard gs
                            Place coord -> updateBoard (putStone (currentBoard gs) coord (Stone (currentPlayer gs))) (currentPlayer gs)
         , currentPlayer = next $ currentPlayer gs
         , lastAction = action -- TODO: keep longer history?
         , previousBoards = currentBoard gs : previousBoards gs -- TODO: don't keep infinite history to save memory?
         , consecutivePasses = case action of
                                 Pass -> succ $ consecutivePasses gs
                                 Place _ -> 0
         , countTurns = succ $ countTurns gs
         }

-- | Check rules. This should be done after applying an 'Action' to the 'GameState' with 'act'.
checkRules :: Game b => Rules -> AssociatedGameState b -> Either RuleViolation (AssociatedGameState b)
checkRules rls gs = flip runReader gs . runRulesT rls $ do
  checkFree
  checkPassing
  checkSuicide
  checkKo
  return gs

-- | Check that the last placed stone was placed on a free coordinate.
checkFree :: (Game b, MonadError RuleViolation m, MonadReader (AssociatedGameState b) m) => m ()
checkFree = do GState {..} <- ask
               case lastAction of
                 Pass -> return ()
                 Place c -> case previousBoards of
                              previousBoard : _ -> when (getStone previousBoard c /= Free) $
                                                     throwError RuleViolationNotFree
                              _ -> return ()

-- | Check if the number of consecutive passes is below the number of players.
checkPassing :: forall b m. (Game b, MonadReader (AssociatedGameState b) m, MonadRules m) => m ()
checkPassing = do GState {..} <- ask
                  rPassing <- passing <$> rules
                  case rPassing of
                    Allowed -> when (consecutivePasses >= natVal (Proxy :: Proxy (AssociatedPlayerCount b))) $
                                 throwError ExceptionEnd
                    Forbidden -> when (lastAction == Pass) $
                                   throwError RuleViolationPassing

-- | Check that the last placed stone wasn't removed right after, because it didn't have any liberties.
checkSuicide :: (Game b, MonadReader (AssociatedGameState b) m, MonadRules m) => m ()
checkSuicide = do GState {..} <- ask
                  rSuicide <- suicide <$> rules
                  case rSuicide of
                    Allowed -> return ()
                    Forbidden -> case lastAction of
                                   Pass -> return ()
                                   Place c -> when (getStone currentBoard c == Free) $
                                                throwError RuleViolationSuicide

-- | Check if the last applied action is correct regarding to the ko-rule 'ko'.
checkKo :: (Game b, MonadReader (AssociatedGameState b) m, MonadRules m) => m ()
checkKo = do GState {..} <- ask
             rKo <- ko <$> rules
             case rKo of
               Ko Allowed -> return ()
               Ko Forbidden -> case previousBoards of
                                 _ : compareBoard : _ -> when (currentBoard == compareBoard) $
                                                           throwError RuleViolationKo
                                 _ -> return ()
               SuperKo -> undefined -- TODO implement carefully
