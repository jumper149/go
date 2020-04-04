{-# LANGUAGE AllowAmbiguousTypes, FlexibleContexts #-}

module Go.Game.Playing ( PlayingT
                       , playPlayingT
                       , play
                       , doTurn
                       ) where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State.Strict

import Go.Game.Config
import Go.Game.Game
import Go.Game.Rules
import Go.Game.State

-- | A monad where the game is played.
newtype PlayingT b c p m a = PlayingT { unwrapPlayingT :: StateT (GameState b c p) (RulesetEnvT m) a }
  deriving (Functor, Applicative, Monad)

instance MonadTrans (PlayingT b c p) where
  lift = PlayingT . lift . lift

-- TODO: Remove this function?
-- | Play some turns and return the GameState at the end.
playPlayingT :: (Game b c p, Monad m, MonadError Malconfig m, MonadReader Config m)
             => PlayingT b c p m ()
             -> m (GameState b c p)
playPlayingT turns = do gs <- initState
                        rls <- asks rules
                        snd <$> runPlayingT rls gs turns

-- | Turns of a whole game.
play :: forall b c p m. (Game b c p, Monad m)
     => (GameState b c p -> m (Either Exception (Action c))) -- ^ get Action
     -> (GameState b c p -> m ())                            -- ^ render Board
     -> PlayingT b c p m ()
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
doTurn :: Game b c p => Rules -> Action c -> GameState b c p -> GameState b c p
doTurn rls action gs = snd . runIdentity $ runPlayingT rls gs turn
  where turn = do PlayingT $ act action
                  problem <- PlayingT checkRules
                  case problem of
                    Right () -> return ()
                    Left ExceptRedo -> PlayingT $ put gs
                    Left ExceptEnd -> undefined -- TODO: undefined behaviour

-- | Helper function for 'play' and 'doTurn'.
runPlayingT :: (Game b c p, Monad m)
            => Rules
            -> GameState b c p
            -> PlayingT b c p m a
            -> m (a,GameState b c p)
runPlayingT rls gs turns = runRulesetEnvT rls $ runStateT (unwrapPlayingT turns) gs
