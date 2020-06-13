{-# LANGUAGE AllowAmbiguousTypes, FlexibleContexts, RecordWildCards #-}

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

-- TODO currently everything is checked after acting!
checkRules :: (Game b c n, Monad m, MonadReader Rules m, MonadState (GameState b c n) m) => m (Either Exception ())
checkRules = runExceptT $ do checkPassing
                             checkFree
                             checkSuicide
                             checkKo

checkPassing :: (Game b c n, Monad m, MonadError Exception m, MonadReader Rules m, MonadState (GameState b c n) m) => m ()
checkPassing = do gs <- get
                  rPassing <- reader passing
                  case rPassing of
                    Allowed -> when (consecutivePasses gs >= countPlayers (currentPlayer gs)) $
                                  throwError ExceptEnd
                    Forbidden -> when (lastAction gs == Pass) $
                                   throwError ExceptRedo

checkFree :: (Game b c r, Monad m, MonadError Exception m, MonadState (GameState b c n) m) => m ()
checkFree = do gs <- get
               case lastAction gs of
                 Pass -> return ()
                 Place c -> when (getStone (head $ previousBoards gs) c /= Free) $ -- TODO unsafe head
                              throwError ExceptRedo

checkSuicide :: (Game b c n, Monad m, MonadError Exception m, MonadReader Rules m, MonadState (GameState b c n) m) => m ()
checkSuicide = do gs <- get
                  rSuicide <- reader suicide
                  case rSuicide of
                    Allowed -> return ()
                    Forbidden -> case lastAction gs of
                                   Pass -> return ()
                                   Place c -> when (getStone (currentBoard gs) c == Free) $
                                                throwError ExceptRedo

-- TODO how does passing and ko work together?
checkKo :: (Game b c n, Monad m, MonadError Exception m, MonadReader Rules m, MonadState (GameState b c n) m) => m ()
checkKo = do gs <- get
             rKo <- reader ko
             case rKo of
               Ko Allowed -> return ()
               Ko Forbidden -> case previousBoards gs of
                                 _ : compareBoard : _ -> when (currentBoard gs == compareBoard) $
                                                           throwError ExceptRedo
                                 _ -> return ()
               SuperKo -> return () -- TODO implement carefully
