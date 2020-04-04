{-# LANGUAGE AllowAmbiguousTypes, FlexibleContexts #-}

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

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

import Go.Game.Config
import Go.Game.Game
import Go.Game.Rules

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

initState :: (Game b c p, Monad m, MonadError Malconfig m, MonadReader Config m)
          => m (GameState b c p)
initState = do emptyBoard <- maybe (throwError MalconfigSize) return =<< asks empty
               return $ GState { currentBoard = emptyBoard
                               , currentPlayer = minBound
                               , lastAction = Pass
                               , previousBoards = [ emptyBoard ]
                               , consecutivePasses = 0
                               , countTurns = 0
                               }

-- | A player can execute the actions represented by this data type.
data Action c = Pass
              | Place c
  deriving (Eq, Show, Generic)

instance (Generic c, FromJSON c) => FromJSON (Action c)
instance (Generic c, ToJSON c) => ToJSON (Action c)

data Exception = ExceptRedo
               | ExceptEnd
  deriving (Eq, Show, Generic)

instance FromJSON Exception
instance ToJSON Exception

-- | Apply action to GameState and handle number of passes. Doesn't check for sanity.
act :: (Game b c p, Monad m, MonadState (GameState b c p) m) => Action c -> m ()
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

-- TODO currently everything is checked after acting!
checkRules :: (Game b c p, Monad m, MonadReader Rules m, MonadState (GameState b c p) m) => m (Either Exception ())
checkRules = runExceptT $ do checkPassing
                             checkFree
                             checkSuicide
                             checkKo

checkPassing :: (Game b c p, Monad m, MonadError Exception m, MonadReader Rules m, MonadState (GameState b c p) m) => m ()
checkPassing = do gs <- get
                  rPassing <- reader passing
                  case rPassing of
                    Allowed -> when (consecutivePasses gs >= countPlayers (currentPlayer gs)) $
                                  throwError ExceptEnd
                    Forbidden -> when (lastAction gs == Pass) $
                                   throwError ExceptRedo

checkFree :: (Game b c p, Monad m, MonadError Exception m, MonadState (GameState b c p) m) => m ()
checkFree = do gs <- get
               case lastAction gs of
                 Pass -> return ()
                 Place c -> when (getStone (head $ previousBoards gs) c /= Free) $ -- TODO unsafe head
                              throwError ExceptRedo

checkSuicide :: (Game b c p, Monad m, MonadError Exception m, MonadReader Rules m, MonadState (GameState b c p) m) => m ()
checkSuicide = do gs <- get
                  rSuicide <- reader suicide
                  case rSuicide of
                    Allowed -> return ()
                    Forbidden -> case lastAction gs of
                                   Pass -> return ()
                                   Place c -> when (getStone (currentBoard gs) c == Free) $
                                                throwError ExceptRedo

-- TODO how does passing and ko work together?
checkKo :: (Game b c p, Monad m, MonadError Exception m, MonadReader Rules m, MonadState (GameState b c p) m) => m ()
checkKo = do gs <- get
             rKo <- reader ko
             case rKo of
               Ko Allowed -> return ()
               Ko Forbidden -> when (currentBoard gs == head (previousBoards gs)) $ -- TODO unsafe head
                                  throwError ExceptRedo
               SuperKo -> return () -- TODO implement carefully

-- | Count the number of players. Helper function for 'checkPassing'.
countPlayers :: forall p. Player p => p -> Int
countPlayers _ = length ([ minBound .. maxBound ] :: [p])

-- | Return the next player. Helper function for 'act'.
next :: Player p => p -> p
next player = if player == maxBound
                 then minBound
                 else succ player
