{-# LANGUAGE FlexibleContexts, RecordWildCards #-}

module Go.Game.Act ( Exception (..)
                   , act
                   , initState
                   ) where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson (FromJSON, ToJSON)
import Data.Proxy
import GHC.Generics
import GHC.TypeLits

import Go.Game.Game
import Go.Game.Player
import Go.Game.Rules
import Go.Game.State

-- | Apply action to 'GameState' and handle all fields in 'GameState'. Doesn't check rules.
act :: forall b c n. Game b c n => Rules -> Action c -> GameState b c n -> Either Exception (GameState b c n)
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

-- | A data type representing exceptions, that may occur in 'checkRules'.
data Exception = ExceptEnd
               | ExceptPassing
               | ExceptNotFree
               | ExceptSuicide
               | ExceptKo
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

instance FromJSON Exception
instance ToJSON Exception

-- | Check rules. This should be done after applying an 'Action' to the 'GameState' with 'act'.
checkRules :: Game b c n => Rules -> GameState b c n -> Either Exception (GameState b c n)
checkRules rls gs = flip runReaderT gs . runRulesetEnvT rls $ do
  checkFree
  checkPassing
  checkSuicide
  checkKo
  return gs

-- | Check that the last placed stone was placed on a free coordinate.
checkFree :: (Game b c n, MonadError Exception m, MonadReader (GameState b c n) m) => m ()
checkFree = do GState {..} <- ask
               case lastAction of
                 Pass -> return ()
                 Place c -> case previousBoards of
                              previousBoard : _ -> when (getStone previousBoard c /= Free) $
                                                     throwError ExceptNotFree
                              _ -> return ()

-- | Check if the number of consecutive passes is below the number of players.
checkPassing :: forall b c m n. (Game b c n, MonadError Exception m, MonadReader (GameState b c n) m, MonadRules m) => m ()
checkPassing = do GState {..} <- ask
                  rPassing <- passing <$> rules
                  case rPassing of
                    Allowed -> when (consecutivePasses >= natVal (Proxy :: Proxy n)) $
                                 throwError ExceptEnd
                    Forbidden -> when (lastAction == Pass) $
                                   throwError ExceptPassing

-- | Check that the last placed stone wasn't removed right after, because it didn't have any liberties.
checkSuicide :: (Game b c n, MonadError Exception m, MonadReader (GameState b c n) m, MonadRules m) => m ()
checkSuicide = do GState {..} <- ask
                  rSuicide <- suicide <$> rules
                  case rSuicide of
                    Allowed -> return ()
                    Forbidden -> case lastAction of
                                   Pass -> return ()
                                   Place c -> when (getStone currentBoard c == Free) $
                                                throwError ExceptSuicide

-- | Check if the last applied action is correct regarding to the ko-rule 'ko'.
checkKo :: (Game b c n, MonadError Exception m, MonadReader (GameState b c n) m, MonadRules m) => m ()
checkKo = do GState {..} <- ask
             rKo <- ko <$> rules
             case rKo of
               Ko Allowed -> return ()
               Ko Forbidden -> case previousBoards of
                                 _ : compareBoard : _ -> when (currentBoard == compareBoard) $
                                                           throwError ExceptKo
                                 _ -> return ()
               SuperKo -> undefined -- TODO implement carefully
