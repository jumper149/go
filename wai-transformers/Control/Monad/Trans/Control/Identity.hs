{-# LANGUAGE FlexibleInstances, FunctionalDependencies, RankNTypes, UndecidableInstances #-}

module Control.Monad.Trans.Control.Identity where

import Control.Monad.Trans.Control
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Reader

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
