{-# LANGUAGE FlexibleContexts, TypeFamilies, UndecidableInstances #-}

module Clients ( ClientsT
               , runClientsT
               , runNewClientsT
               ) where

import Control.Monad.Base
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Trans.Control.Identity
import Control.Monad.Trans.Reader
import GHC.Conc
import GHC.Generics

import Clients.Class

newtype ClientsT n m a = ClientsT { unwrapClientsT :: ReaderT (TVar (Clients n)) m a }
  deriving (Applicative, Functor, Generic, Monad, MonadTrans, MonadTransControl, MonadTransControlIdentity, MonadTransFunctor)

instance MonadBase base m => MonadBase base (ClientsT n m) where
  liftBase = liftBaseDefault

instance MonadBaseControl base m => MonadBaseControl base (ClientsT n m) where
  type StM (ClientsT n m) a = ComposeSt (ClientsT n) m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM

instance MonadBaseControlIdentity base m => MonadBaseControlIdentity base (ClientsT n m) where
  liftBaseWithIdentity inner = ClientsT $ liftBaseWithIdentity $ \ run -> inner $ run . unwrapClientsT

instance Monad m => MonadClients n (ClientsT n m) where
  clientsTVar = ClientsT $ ask

runClientsT :: TVar (Clients n)
            -> ClientsT n m a
            -> m a
runClientsT cs = flip runReaderT cs . unwrapClientsT

runNewClientsT :: MonadBase IO m
               => ClientsT n m a
               -> m (a, Clients n)
runNewClientsT ct = do clientsTVar' <- liftBase $ newTVarIO mempty
                       val <- runClientsT clientsTVar' ct
                       cs <- liftBase $ readTVarIO clientsTVar'
                       return (val , cs)
