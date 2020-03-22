{-# LANGUAGE AllowAmbiguousTypes, FlexibleContexts #-}

module State ( PlayingT
             , GameState (..)
             , Action (..)
             , doTurn
             , initState
             , runPlayingT
             , play
             ) where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State.Strict

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

import Game
import Rules

-- | This data type contains the current board, the current player, the previous board and the
-- number of consecutive passes.
data GameState b c p = GState { currentBoard :: b
                              , currentPlayer :: p
                              , lastAction :: Action c
                              , previousBoards :: [b]
                              , consecutivePasses :: Int
                              , countTurns :: Int
                              }
                              deriving Generic

instance (Generic b, Generic c, Generic p, FromJSON b, FromJSON c, FromJSON p) => FromJSON (GameState b c p)
instance (Generic b, Generic c, Generic p, ToJSON b, ToJSON c, ToJSON p) => ToJSON (GameState b c p)

initState :: Game b c p => GameState b c p
initState = GState { currentBoard = empty
                   , currentPlayer = minBound
                   , lastAction = Pass
                   , previousBoards = [ empty ]
                   , consecutivePasses = 0
                   , countTurns = 0
                   }

-- | A player can execute the actions represented by this data type.
data Action c = Pass
              | Place c
              deriving (Eq,Generic)

instance (Generic c, FromJSON c) => FromJSON (Action c)
instance (Generic c, ToJSON c) => ToJSON (Action c)

data Exception = ExceptRedo
               | ExceptEnd

newtype PlayingT b c p m a = PlayingT { unwrapPlayingT :: StateT (GameState b c p) (ExceptT Exception (RulesetEnvT m)) a }
    deriving (Functor, Applicative, Monad, MonadState (GameState b c p), MonadError Exception, MonadReader Rules)

instance MonadTrans (PlayingT b c p) where
    lift = PlayingT . lift . lift . lift

-- | Apply action to GameState and handle number of passes. Doesn't check for sanity.
act :: (Game b c p, Monad m) => Action c -> PlayingT b c p m ()
act action = do gs <- get
                let previousBoard = currentBoard gs
                    actedGs = case action of
                                Pass -> let newConsecutivePasses = consecutivePasses gs + 1
                                        in gs { lastAction = Pass
                                              , consecutivePasses = newConsecutivePasses
                                              }
                                Place c -> let newB = updateBoard (putStone (currentBoard gs) c (Stone (currentPlayer gs))) (currentPlayer gs)
                                           in gs { currentBoard = newB
                                                 , lastAction = Place c
                                                 , previousBoards = [ previousBoard ] -- TODO keep history
                                                 , consecutivePasses = 0
                                                 }
                    correctedGs = actedGs { currentPlayer = next $ currentPlayer gs
                                          , countTurns = countTurns gs + 1
                                          }
                put correctedGs

checkRules :: (Game b c p, Monad m) => PlayingT b c p m ()
checkRules = do checkPassing
                checkFree
                checkSuicide
                checkKo

-- TODO currently everything is checked after acting!
checkPassing :: (Game b c p, Monad m) => PlayingT b c p m ()
checkPassing = do gs <- get
                  rPassing <- reader passing
                  case rPassing of
                    Allowed -> unless (consecutivePasses gs < countPlayers (currentPlayer gs)) $
                                 throwError ExceptEnd
                    Forbidden -> when (lastAction gs == Pass) $
                                   throwError ExceptRedo

checkFree :: (Game b c p, Monad m) => PlayingT b c p m ()
checkFree = do gs <- get
               case lastAction gs of
                 Pass -> return ()
                 Place c -> unless (getStone (head $ previousBoards gs) c == Free) $ -- TODO unsafe head
                              throwError ExceptRedo

checkSuicide :: (Game b c p, Monad m) => PlayingT b c p m ()
checkSuicide = do gs <- get
                  rSuicide <- reader suicide
                  case rSuicide of
                    Allowed -> return ()
                    Forbidden -> case lastAction gs of
                                   Pass -> return ()
                                   Place c -> when (getStone (currentBoard gs) c == Free) $
                                                throwError ExceptRedo

checkKo :: (Game b c p, Monad m) => PlayingT b c p m () -- TODO how does passing and ko work together?
checkKo = do gs <- get
             rKo <- reader ko
             case rKo of
               Ko Allowed -> return ()
               Ko Forbidden -> when (currentBoard gs == head (previousBoards gs)) $ -- TODO unsafe head
                                 throwError ExceptRedo
               SuperKo -> return () -- TODO implement

runPlayingT :: (Game b c p, Monad m)
            => Rules
            -> PlayingT b c p m (GameState b c p)
            -> m (Either Exception (GameState b c p))
runPlayingT rules turn = runRulesetEnvT rules . runExceptT $ evalStateT (unwrapPlayingT $ turn) initState
-- TODO change turn to turns

-- TODO needs to be the whole game
play :: (Game b c p, Monad m)
     => PlayingT b c p m (Action c) -- ^ get Action
     -> PlayingT b c p m () -- ^ render Board
     -> PlayingT b c p m (GameState b c p)
play action render = do render
                        action >>= act
                        checkRules
                        get

-- | Apply action to a GameState.
doTurn :: Game b c p => Rules -> Action c -> GameState b c p -> Either Exception (GameState b c p)
doTurn rules action gs = runIdentity . runRulesetEnvT rules . runExceptT $ evalStateT (unwrapPlayingT turn') gs
  where turn' = do act action
                   checkRules
                   get

-- | Return the next player. Helper function for 'act'.
next :: Player p => p -> p
next player = if player == maxBound
                 then minBound
                 else succ player

-- | Count the number of players. Helper function for 'checkPassing'.
countPlayers :: forall p. Player p => p -> Int
countPlayers _ = length ([ minBound .. maxBound ] :: [p])