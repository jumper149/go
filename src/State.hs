{-# LANGUAGE AllowAmbiguousTypes, FlexibleContexts, GeneralizedNewtypeDeriving #-}

module State ( PlayingT (..)
             , MonadPlaying ( draw
                            , getAction
                            , access
                            )
             , GameState (..)
             , Action (..)
             , Exception (..)
             , EndScreen (..)
             , start
             ) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict

import Data.Either (fromRight)

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

-- | A player can execute the actions represented by this data type.
data Action c = Pass
              | Place c
              deriving (Eq)

data Exception = Redo
               | End

-- | Holds informaion for the endscreen.
data EndScreen b p = EndScreen { lastBoard :: b
                               , winner :: p
                               , points :: [(p,Int)]
                               , stonesOnBoard :: [(p,Int)]
                               , turns :: Int
                               }

newtype PlayingT b c p m a = PlayingT (StateT (GameState b c p) (ExceptT Exception (RulesetEnvT m)) a)
    deriving (Functor, Applicative, Monad, MonadState (GameState b c p), MonadError Exception,
              MonadReader Rules)

instance MonadTrans (PlayingT b c p) where
    lift = PlayingT . lift . lift . lift

class (Game b c p, Monad m) => MonadPlaying b c p m where

    getAction :: PlayingT b c p m (Action c)

    draw :: PlayingT b c p m ()

    access :: (GameState b c p -> a) -> PlayingT b c p m a
    access = gets

    -- | Safely apply an action to the state of a game.
    turn :: PlayingT b c p m (EndScreen b p)
    turn = do gs <- get
              draw
              action <- getAction
              act action
              checkRules
              let handler Redo = put gs >> turn
                  handler End = finalize <$> get
              gets finalize `catchError` handler
              turn

    -- | Apply action to GameState and handle number of passes. Doesn't check for sanity.
    act :: Action c -> PlayingT b c p m ()
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
                        correctedGs = actedGs { currentPlayer = next $ currentPlayer gs -- TODO use monad instead of arguments
                                              , countTurns = countTurns gs + 1 -- TODO same
                                              }
                    put correctedGs

    checkRules :: PlayingT b c p m ()
    checkRules = do checkPassing
                    checkFree
                    checkSuicide
                    checkKo

    -- TODO currently everything is checked after acting!
    checkPassing :: PlayingT b c p m ()
    checkPassing = do gs <- get
                      rPassing <- reader passing
                      case rPassing of
                        Allowed -> unless (consecutivePasses gs < countPlayers (currentPlayer gs)) $
                                     throwError End
                        Forbidden -> when (lastAction gs == Pass) $
                                       throwError Redo

    checkFree :: PlayingT b c p m ()
    checkFree = do gs <- get
                   case lastAction gs of
                     Pass -> return ()
                     Place c -> unless (getStone (head $ previousBoards gs) c == Free) $ -- TODO unsafe head
                                  throwError Redo

    checkSuicide :: PlayingT b c p m ()
    checkSuicide = do gs <- get
                      rSuicide <- reader suicide
                      case rSuicide of
                        Allowed -> return ()
                        Forbidden -> case lastAction gs of
                                       Pass -> return ()
                                       Place c -> when (getStone (currentBoard gs) c == Free) $
                                                    throwError Redo

    checkKo :: PlayingT b c p m () -- TODO how does passing and ko work together?
    checkKo = do gs <- get
                 rKo <- reader ko
                 case rKo of
                   Ko Allowed -> return ()
                   Ko Forbidden -> when (currentBoard gs == head (previousBoards gs)) $ -- TODO unsafe head
                                     throwError Redo
                   SuperKo -> return () -- TODO implement

-- TODO no undefined shit
startPlaying :: forall b c p m. MonadPlaying b c p m => Rules ->  m (EndScreen b p)
startPlaying rules = fromRight undefined <$> runRulesetEnvT rules (runExceptT (evalStateT play initState))
  where initState = GState { currentBoard = empty
                           , currentPlayer = minBound
                           , lastAction = Pass
                           , previousBoards = [ empty ]
                           , consecutivePasses = 0
                           , countTurns = 0
                           }
        PlayingT play = turn

-- TODO nicer
start :: forall b c p m. MonadPlaying b c p m => m (EndScreen b p)
start = startPlaying defaultRules

-- | Transform the state of a finished game to the endscreen.
finalize :: forall b c p. Game b c p => GameState b c p -> EndScreen b p
finalize gs = EndScreen { lastBoard = currentBoard gs
                        , winner = currentPlayer gs
                        , points = []
                        , stonesOnBoard = map (\ x -> (x , countStones (currentBoard gs) x)) ([ minBound .. maxBound ] :: [p])
                        , turns = countTurns gs
                        }

-- | Return the next player.
next :: Player p => p -> p
next player = if player == maxBound
              then minBound
              else succ player

-- | Count the number of players.
countPlayers :: forall p. Player p => p -> Int
countPlayers _ = length ([ minBound .. maxBound ] :: [p])

-- | Count the number of stones a player has on the board.
countStones :: forall b c p. Game b c p => b -> p -> Int
countStones board player = length $ filter hasPlayerStone $ coords board
  where hasPlayerStone :: c -> Bool
        hasPlayerStone coord = getStone board coord == Stone player
