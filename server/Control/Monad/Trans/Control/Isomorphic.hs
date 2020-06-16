{-# LANGUAGE FlexibleInstances, FunctionalDependencies, RankNTypes, UndecidableInstances #-}

module Control.Monad.Trans.Control.Isomorphic where

import Control.Monad.Trans.Control
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Reader

-- | Instances of this class are the same as instances of 'MonadUnliftIO', but for any base monad.
class MonadBaseControl b m => MonadBaseControlIsomorphic b m | m -> b where
  withRunInBase :: ((forall a. m a -> b a) -> b c) -> m c

instance MonadBaseControl b b => MonadBaseControlIsomorphic b b where
  withRunInBase inner = inner id

instance MonadBaseControlIsomorphic b m => MonadBaseControlIsomorphic b (IdentityT m) where
  withRunInBase inner = IdentityT $ withRunInBase $ \ run ->
    inner (run . runIdentityT)

instance MonadBaseControlIsomorphic b m => MonadBaseControlIsomorphic b (ReaderT r m) where
  withRunInBase inner = ReaderT $ \ r ->
    withRunInBase $ \ run ->
      inner (run . flip runReaderT r)
