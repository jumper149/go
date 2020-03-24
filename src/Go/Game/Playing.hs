{-# LANGUAGE AllowAmbiguousTypes, FlexibleContexts #-}

module Go.Game.Playing ( PlayingT
                       , runPlayingT
                       , play
                       , doTurn
                       ) where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State.Strict

import Go.Game.Game
import Go.Game.Rules
import Go.Game.State

class (Game b c p, Monad m) => MonadPlaying b c p m where

    -- | Retrieve current GameState.
    gamestate :: m (GameState b c p)

newtype PlayingT b c p m a = PlayingT { unwrapPlayingT :: StateT (GameState b c p) (RulesetEnvT m) a }
    deriving (Functor, Applicative, Monad)

instance MonadTrans (PlayingT b c p) where
    lift = PlayingT . lift . lift

instance (Game b c p, Monad m) => MonadPlaying b c p (PlayingT b c p m) where
    gamestate = PlayingT get

runPlayingT :: (Game b c p, Monad m)
            => Rules
            -> PlayingT b c p m (GameState b c p)
            -> m (GameState b c p)
runPlayingT rules turn = runPlayingTRaw rules initState turn
-- TODO change turn to turns EVERYWHERE!!!

-- | Play a whole game.
play :: forall b c p m. (Game b c p, Monad m)
     => (GameState b c p -> m (Either Exception (Action c))) -- ^ get Action
     -> (GameState b c p -> m ()) -- ^ render Board
     -> PlayingT b c p m (GameState b c p)
play action render = do gs <- gamestate
                        let resetGame = PlayingT (put gs)
                        lift $ render gs
                        mbAction <- lift (action gs)
                        case mbAction of
                          Right actn -> do PlayingT $ act actn
                                           problem <- PlayingT checkRules
                                           case problem of
                                             Right () -> rec
                                             Left ExceptRedo -> resetGame >> rec
                                             Left ExceptEnd -> return gs -- TODO return gs or get
                          Left ExceptRedo -> resetGame >> rec
                          Left ExceptEnd -> undefined
  where rec = play action render

-- | Apply action to a GameState.
doTurn :: Game b c p => Rules -> Action c -> GameState b c p -> GameState b c p
doTurn rules action gs = runIdentity $ runPlayingTRaw rules gs turn
  where turn = do gs <- gamestate
                  let resetGame = PlayingT (put gs)
                  PlayingT $ act action
                  problem <- PlayingT checkRules
                  case problem of
                    Right () -> gamestate
                    Left ExceptRedo -> resetGame >> gamestate
                    Left ExceptEnd -> undefined

-- | Helper function for 'play' and 'doTurn'.
runPlayingTRaw :: (Game b c p, Monad m)
               => Rules
               -> GameState b c p
               -> PlayingT b c p m (GameState b c p)
               -> m (GameState b c p)
runPlayingTRaw rules gs turn = runRulesetEnvT rules $ evalStateT (unwrapPlayingT turn) gs
