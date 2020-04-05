{-# LANGUAGE AllowAmbiguousTypes, FlexibleContexts #-}

module Go.Game.Playing ( PlayingT
                       , runPlayingT
                       , execPlayingT
                       , play
                       , doTurn
                       ) where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State.Strict

import Go.Game.Game
import Go.Game.Player
import Go.Game.Rules
import Go.Game.State

-- | A monad where the game is played.
newtype PlayingT b c n m a = PlayingT { unwrapPlayingT :: StateT (GameState b c n) (RulesetEnvT m) a }
  deriving (Functor, Applicative, Monad)

instance MonadTrans (PlayingT b c n) where
  lift = PlayingT . lift . lift

-- | Play some turns in 'PlayingT' and evaluate them.
runPlayingT :: (Game b c n, Monad m)
            => Rules
            -> GameState b c n
            -> PlayingT b c n m a
            -> m (a,GameState b c n)
runPlayingT rls gs turns = runRulesetEnvT rls $ runStateT (unwrapPlayingT turns) gs

-- | Play some turns in 'PlayingT' and return 'GameState' at the end.
execPlayingT :: (Game b c n, Monad m)
             => Rules
             -> GameState b c n
             -> PlayingT b c n m a
             -> m (GameState b c n)
execPlayingT rls gs turns = snd <$> runPlayingT rls gs turns

-- | Turns of a whole game.
play :: forall b c n m. (Game b c n, Monad m)
     => (GameState b c n -> m (Either Exception (Action c))) -- ^ get Action
     -> (GameState b c n -> m ())                            -- ^ render Board
     -> PlayingT b c n m ()
play action render = do gs <- PlayingT get
                        let resetGame = PlayingT (put gs)
                        lift $ render gs
                        mbAction <- lift (action gs)
                        case mbAction of
                          Right actn -> do PlayingT $ act actn
                                           problem <- PlayingT checkRules
                                           case problem of
                                             Right () -> rec
                                             Left ExceptRedo -> resetGame >> rec
                                             Left ExceptEnd -> return () -- TODO return gs or get
                          Left ExceptRedo -> resetGame >> rec
                          Left ExceptEnd -> undefined -- TODO: undefined behaviour
  where rec = play action render

-- | Apply action to a GameState.
doTurn :: Game b c n => Rules -> Action c -> GameState b c n -> GameState b c n
doTurn rls action gs = snd . runIdentity $ runPlayingT rls gs turn
  where turn = do PlayingT $ act action
                  problem <- PlayingT checkRules
                  case problem of
                    Right () -> return ()
                    Left ExceptRedo -> PlayingT $ put gs
                    Left ExceptEnd -> undefined -- TODO: undefined behaviour
