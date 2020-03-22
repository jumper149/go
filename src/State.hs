{-# LANGUAGE AllowAmbiguousTypes, FlexibleContexts #-}

module State ( PlayingT
             , MonadPlaying ( getAction
                            , drawGame
                            , drawEnd
                            , access
                            )
             , GameState (..)
             , Action (..)
             , EndScreen (..)
             , start
             , doTurn
             , initState
             , finalizeState
             ) where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State.Strict

import Data.Either (fromRight)
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

-- | Holds informaion for the endscreen.
data EndScreen b p = EndScreen { lastBoard :: b
                               , winner :: p
                               , points :: [(p,Int)]
                               , stonesOnBoard :: [(p,Int)]
                               , turns :: Int
                               }

-- | Transform the state of a finished game to the endscreen.
finalizeState :: forall b c p. Game b c p => GameState b c p -> EndScreen b p
finalizeState gs = EndScreen { lastBoard = currentBoard gs
                             , winner = currentPlayer gs
                             , points = []
                             , stonesOnBoard = map (\ x -> (x , countStones (currentBoard gs) x)) ([ minBound .. maxBound ] :: [p])
                             , turns = countTurns gs
                             }

newtype PlayingT b c p m a = PlayingT { unwrapPlayingT :: StateT (GameState b c p) (ExceptT Exception (RulesetEnvT m)) a }
    deriving (Functor, Applicative, Monad, MonadState (GameState b c p), MonadError Exception,
              MonadReader Rules)

instance MonadTrans (PlayingT b c p) where
    lift = PlayingT . lift . lift . lift

class (Game b c p, Monad m) => MonadPlaying b c p m where

    getAction :: PlayingT b c p m (Action c) -- TODO what happens if this fails?

    drawGame :: PlayingT b c p m ()

    drawEnd :: EndScreen b p -> m ()

    access :: (GameState b c p -> a) -> PlayingT b c p m a
    access = gets

    -- | Safely apply an action to the state of a game.
    turn :: PlayingT b c p m (EndScreen b p) -- TODO rework all of this!!!
    turn = do gs <- get
              drawGame
              action <- getAction
              act action
              checkRules
              let handler ExceptRedo = put gs >> turn
                  handler ExceptEnd = finalizeState <$> get
              gets finalizeState `catchError` handler
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
                        correctedGs = actedGs { currentPlayer = next $ currentPlayer gs
                                              , countTurns = countTurns gs + 1
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
                                     throwError ExceptEnd
                        Forbidden -> when (lastAction gs == Pass) $
                                       throwError ExceptRedo

    checkFree :: PlayingT b c p m ()
    checkFree = do gs <- get
                   case lastAction gs of
                     Pass -> return ()
                     Place c -> unless (getStone (head $ previousBoards gs) c == Free) $ -- TODO unsafe head
                                  throwError ExceptRedo

    checkSuicide :: PlayingT b c p m ()
    checkSuicide = do gs <- get
                      rSuicide <- reader suicide
                      case rSuicide of
                        Allowed -> return ()
                        Forbidden -> case lastAction gs of
                                       Pass -> return ()
                                       Place c -> when (getStone (currentBoard gs) c == Free) $
                                                    throwError ExceptRedo

    checkKo :: PlayingT b c p m () -- TODO how does passing and ko work together?
    checkKo = do gs <- get
                 rKo <- reader ko
                 case rKo of
                   Ko Allowed -> return ()
                   Ko Forbidden -> when (currentBoard gs == head (previousBoards gs)) $ -- TODO unsafe head
                                     throwError ExceptRedo
                   SuperKo -> return () -- TODO implement

-- TODO no undefined shit
start :: forall b c p m. MonadPlaying b c p m => Rules -> m (EndScreen b p)
start rules = fromRight undefined <$> runRulesetEnvT rules (runExceptT (evalStateT (unwrapPlayingT turn) initState))

-- TODO HACKY af! needed turn' in doTurn
instance Game b c p => MonadPlaying b c p Identity where
    getAction = undefined
    drawGame = undefined
    drawEnd = undefined

doTurn :: Game b c p => Rules -> Action c -> GameState b c p -> Either Exception (GameState b c p)
doTurn rules action gs = runIdentity $ runRulesetEnvT rules $ runExceptT $ evalStateT (unwrapPlayingT $ turn') gs
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

-- | Count the number of stones a player has on the board. Helper function for 'finalizeState'.
countStones :: forall b c p. Game b c p => b -> p -> Int
countStones board player = length $ filter hasPlayerStone $ coords board
  where hasPlayerStone coord = getStone board coord == Stone player
