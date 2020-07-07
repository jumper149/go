{-# LANGUAGE UndecidableInstances #-}

module Clients ( ClientsT
               , runClientsT
               ) where

import Control.Monad.Base
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Trans.Control.Identity
import Control.Monad.Trans.Reader
import GHC.Conc
import GHC.Generics

import Clients.Class

newtype ClientsT m a = ClientsT { unwrapClientsT :: ReaderT (TVar Clients) m a }
  deriving (Applicative, Functor, Generic, Monad, MonadTrans, MonadTransControl, MonadTransControlIdentity, MonadTransFunctor)

instance MonadBase base m => MonadBase base (ClientsT m) where
  liftBase = liftBaseDefault

instance MonadBaseControl base m => MonadBaseControl base (ClientsT m) where
  type StM (ClientsT m) a = ComposeSt ClientsT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM

instance MonadBaseControlIdentity base m => MonadBaseControlIdentity base (ClientsT m) where
  liftBaseWithIdentity = defaultLiftBaseWithIdentity

instance Monad m => MonadClients (ClientsT m) where
  clientsTVar = ClientsT $ ask

runClientsT :: TVar Clients
            -> ClientsT m a
            -> m a
runClientsT cs = flip runReaderT cs . unwrapClientsT
