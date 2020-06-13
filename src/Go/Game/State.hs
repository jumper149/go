{-# LANGUAGE FlexibleContexts, RecordWildCards #-}

module Go.Game.State ( GameState (..)
                     , Action (..)
                     , Exception (..)
                     , act
                     , checkRules
                     , initState
                     ) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import GHC.TypeLits

import Go.Game.Config
import Go.Game.Game
import Go.Game.Player
import Go.Game.Rules

-- | This data type contains the current board, the current player, the previous board and the
-- number of consecutive passes.
data GameState b c n = GState { currentBoard :: b
                              , currentPlayer :: PlayerN n
                              , lastAction :: Action c
                              , previousBoards :: [b]
                              , consecutivePasses :: Int
                              , countTurns :: Int
                              }
  deriving (Eq, Generic, Ord, Read, Show)

instance (KnownNat n, Generic b, Generic c, FromJSON b, FromJSON c) => FromJSON (GameState b c n)
instance (KnownNat n, Generic b, Generic c, ToJSON b, ToJSON c) => ToJSON (GameState b c n)

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

instance (Generic c, FromJSON c) => FromJSON (Action c)
instance (Generic c, ToJSON c) => ToJSON (Action c)

data Exception = ExceptRedo
               | ExceptEnd
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

instance FromJSON Exception
instance ToJSON Exception

-- | Apply action to 'GameState' and handle all fields in 'GameState'. Doesn't check rules.
act :: forall b c n m. (Game b c n, Monad m, MonadState (GameState b c n) m) => Action c -> m ()
act action = do gs <- get
                put GState { currentBoard = case action of
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
checkRules :: (Game b c n, Monad m, MonadReader Rules m, MonadState (GameState b c n) m) => m (Either Exception ())
checkRules = runExceptT $ do checkFree
                             checkPassing
                             checkSuicide
                             checkKo

-- | Check that the last placed stone was placed on a free coordinate.
checkFree :: (Game b c r, Monad m, MonadError Exception m, MonadState (GameState b c n) m) => m ()
checkFree = do GState {..} <- get
               case lastAction of
                 Pass -> return ()
                 Place c -> case previousBoards of
                              previousBoard : _ -> when (getStone previousBoard c /= Free) $
                                                     throwError ExceptRedo
                              _ -> return ()

-- | Check if the number of consecutive passes is below the number of players.
checkPassing :: (Game b c n, Monad m, MonadError Exception m, MonadReader Rules m, MonadState (GameState b c n) m) => m ()
checkPassing = do GState {..} <- get
                  rPassing <- reader passing
                  case rPassing of
                    Allowed -> when (consecutivePasses >= countPlayers (currentPlayer)) $
                                 throwError ExceptEnd
                    Forbidden -> when (lastAction == Pass) $
                                   throwError ExceptRedo

-- | Check that the last placed stone wasn't removed right after, because it didn't have any liberties.
checkSuicide :: (Game b c n, Monad m, MonadError Exception m, MonadReader Rules m, MonadState (GameState b c n) m) => m ()
checkSuicide = do GState {..} <- get
                  rSuicide <- reader suicide
                  case rSuicide of
                    Allowed -> return ()
                    Forbidden -> case lastAction of
                                   Pass -> return ()
                                   Place c -> when (getStone (currentBoard) c == Free) $
                                                throwError ExceptRedo

-- | Check if the last applied action is correct regarding to the ko-rule 'ko'.
checkKo :: (Game b c n, Monad m, MonadError Exception m, MonadReader Rules m, MonadState (GameState b c n) m) => m ()
checkKo = do GState {..} <- get
             rKo <- reader ko
             case rKo of
               Ko Allowed -> return ()
               Ko Forbidden -> case previousBoards of
                                 _ : compareBoard : _ -> when (currentBoard == compareBoard) $
                                                           throwError ExceptRedo -- TODO: use more explicit error type
                                 _ -> return ()
               SuperKo -> return () -- TODO implement carefully
