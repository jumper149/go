{-# LANGUAGE FlexibleInstances, FunctionalDependencies, RankNTypes, UndecidableInstances #-}

module Control.Monad.Trans.Control.Identity where

import Control.Monad.Trans.Control
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Reader

class MonadTransControl t => MonadTransControlIdentity t where
  liftWithIdentity :: ((forall a. t m a -> m a) -> m b) -> t m b

instance MonadTransControlIdentity IdentityT where
  liftWithIdentity inner = IdentityT $ inner runIdentityT

instance MonadTransControlIdentity (ReaderT r) where
  liftWithIdentity inner = ReaderT $ \ r -> inner $ flip runReaderT r

-- | Instances of this class are the same as instances of 'MonadUnliftIO', but for any base monad.
class MonadBaseControl b m => MonadBaseControlIdentity b m | m -> b where
  liftBaseWithIdentity :: ((forall a. m a -> b a) -> b c) -> m c

instance MonadBaseControl b b => MonadBaseControlIdentity b b where
  liftBaseWithIdentity inner = inner id

instance MonadBaseControlIdentity b m => MonadBaseControlIdentity b (IdentityT m) where
  liftBaseWithIdentity inner = IdentityT $ liftBaseWithIdentity $ \ run ->
    inner $ run . runIdentityT

instance MonadBaseControlIdentity b m => MonadBaseControlIdentity b (ReaderT r m) where
  liftBaseWithIdentity inner = ReaderT $ \ r ->
    liftBaseWithIdentity $ \ run ->
      inner $ run . flip runReaderT r

class MonadTransControlIdentity t => MonadTransFunctor t where
  mapT :: (m a -> n b) -> t m a -> t n b

instance MonadTransFunctor IdentityT where
  mapT f = IdentityT . f . runIdentityT

instance MonadTransFunctor (ReaderT r) where
  mapT f m = ReaderT $ f . runReaderT m
