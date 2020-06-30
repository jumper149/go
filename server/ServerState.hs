{-# LANGUAGE FlexibleContexts, TypeFamilies, UndecidableInstances #-}

module ServerState ( ServerStateT
                   , runServerStateT
                   , runNewServerStateT
                   ) where

import Control.Monad.Base
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Trans.Control.Identity
import Control.Monad.Trans.Reader
import GHC.Conc
import GHC.Generics

import Clients
import Clients.Class
import ServerState.Class

import Go.Game.Config
import Go.Game.Game
import Go.Game.State

data ServerState b c n = ServerState { gameStateTVar' :: TVar (GameState b c n)
                                     , gameConfig' :: Config
                                     }
  deriving (Eq, Generic)

newtype ServerStateT b c n m a = ServerStateT { unwrapServerStateT :: ReaderT (ServerState b c n) (ClientsT n m) a }
  deriving (Applicative, Functor, Generic, Monad, MonadClients n)

instance MonadTrans (ServerStateT b c n) where
  lift = ServerStateT . lift . lift

instance MonadTransControl (ServerStateT b c n) where
  type StT (ServerStateT b c n) a = StT (ReaderT (ServerState b c n)) (StT (ClientsT n) a)
  liftWith = defaultLiftWith2 ServerStateT unwrapServerStateT
  restoreT = defaultRestoreT2 ServerStateT

instance MonadTransControlIdentity (ServerStateT b c n) where
  liftWithIdentity inner = ServerStateT $ liftWithIdentity $ \ runReaderT ->
                             liftWithIdentity $ \ runClientsT ->
                               inner $ runClientsT . runReaderT . unwrapServerStateT

instance MonadTransFunctor (ServerStateT b c n) where
  mapT f = ServerStateT . mapT (mapT f) . unwrapServerStateT

instance MonadBase base m => MonadBase base (ServerStateT b c n m) where
  liftBase = liftBaseDefault

instance MonadBaseControl base m => MonadBaseControl base (ServerStateT b c n m) where
  type StM (ServerStateT b c n m) a = ComposeSt (ServerStateT b c n) m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM

instance MonadBaseControlIdentity base m => MonadBaseControlIdentity base (ServerStateT b c n m) where
  liftBaseWithIdentity inner = ServerStateT $ liftBaseWithIdentity $ \ run -> inner $ run . unwrapServerStateT

instance Monad m => MonadServerState b c n (ServerStateT b c n m) where
  gameStateTVar = ServerStateT $ reader gameStateTVar'
  gameConfig = ServerStateT $ reader gameConfig'

runServerStateT :: TVar (Clients n)
                -> ServerState b c n
                -> ServerStateT b c n m a
                -> m a
runServerStateT cs ss = runClientsT cs . flip runReaderT ss . unwrapServerStateT

runNewServerStateT :: (Game b c n, MonadBase IO m)
                   => Config
                   -> ServerStateT b c n m a
                   -> m (a, Clients n, GameState b c n)
runNewServerStateT config sst = do gameStateTVar' <- liftBase $ newTVarIO initState
                                   let ss = ServerState { gameStateTVar' = gameStateTVar'
                                                        , gameConfig' = config
                                                        }
                                   (val,cs) <- runNewClientsT . flip runReaderT ss $ unwrapServerStateT sst
                                   gs <- liftBase $ readTVarIO gameStateTVar'
                                   return (val,cs,gs)
