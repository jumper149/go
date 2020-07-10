{-# LANGUAGE UndecidableInstances, RecordWildCards #-}

module GameSet ( GameSetT
               , runGameSetT
               ) where

import Control.Monad.Base
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Trans.Control.Identity
import Control.Monad.Trans.Reader
import GHC.Conc
import GHC.Generics

import Clients
import GameSet.Class
import GameSet.Internal
import ServerState
import ServerState.Class
import ServerState.Internal

import Go.Game.Game
import Go.Game.State

data Identification = Identification { clientId :: ClientId
                                     , gameId :: GameId
                                     }
  deriving (Eq, Generic, Ord, Read, Show)

newtype GameSetT m a = GameSetT { unwrapGameSetT :: ReaderT Identification (ServerStateT m) a }
  deriving (Applicative, Functor, Generic, Monad)

instance MonadTrans GameSetT where
  lift = GameSetT . lift . lift

instance MonadTransControl GameSetT where
  type StT GameSetT a = StT (ReaderT Identification) (StT ServerStateT a)
  liftWith = defaultLiftWith2 GameSetT unwrapGameSetT
  restoreT = defaultRestoreT2 GameSetT

instance MonadTransControlIdentity GameSetT where
  liftWithIdentity = defaultLiftWithIdentity

instance MonadTransFunctor GameSetT where
  mapT f = GameSetT . mapT (mapT f) . unwrapGameSetT

instance MonadBase base m => MonadBase base (GameSetT m) where
  liftBase = liftBaseDefault

instance MonadBaseControl base m => MonadBaseControl base (GameSetT m) where
  type StM (GameSetT m) a = ComposeSt GameSetT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM

instance MonadBaseControlIdentity base m => MonadBaseControlIdentity base (GameSetT m) where
  liftBaseWithIdentity = defaultLiftBaseWithIdentity

instance MonadBase STM m => MonadGameSet (GameSetT m) where
  readPlayer = do c <- GameSetT $ reader clientId
                  GameSetT $ getClient c
  readPlayers = do cs <- playerListFrom <$> readGameSet
                   GameSetT . lift $ traverse (getClient . fst) cs -- TODO
  readGameSet = do gs <- GameSetT $ reader gameId
                   GameSetT $ getGameSetFrom gs <$> lift readGameSets -- TODO: remove lifts, by implementing MonadServerState instance for ReaderT
  writeGameSet gs = do gss <- GameSetT $ addGameSetTo gs <$> lift readGameSets -- TODO
                       GameSetT . lift $ writeGameSets gss -- TODO

runGameSetT :: ClientId
            -> GameId
            -> GameSetT m a
            -> ServerStateT m a
runGameSetT clientId gameId = flip runReaderT Identification {..} . unwrapGameSetT
