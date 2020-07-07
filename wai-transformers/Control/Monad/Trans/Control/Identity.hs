{-# LANGUAGE FlexibleInstances, FunctionalDependencies, TypeFamilies, Rank2Types, UndecidableInstances #-}

module Control.Monad.Trans.Control.Identity ( MonadTransControlIdentity (..)
                                            , defaultLiftWithIdentity
                                            , MonadBaseControlIdentity (..)
                                            , defaultLiftBaseWithIdentity
                                            , MonadTransFunctor (..)
                                            , liftTrans
                                            ) where

import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Reader

class MonadTransControl t => MonadTransControlIdentity t where
  liftWithIdentity :: Monad m => ((RunIdentity t) -> m b) -> t m b

type RunIdentity t = forall n b. Monad n => t n b -> n b

type RunIdentityDefault t = forall n b. (Monad n, StT t b ~ b) => t n b -> n b

defaultLiftWithIdentity :: (Monad m, MonadTransControl t)
                        => ((RunIdentityDefault t) -> m b)
                        -> t m b
defaultLiftWithIdentity = liftWith

instance MonadTransControlIdentity IdentityT where
  liftWithIdentity = defaultLiftWithIdentity

instance MonadTransControlIdentity (ReaderT r) where
  liftWithIdentity = defaultLiftWithIdentity

-- | Instances of this class are the same as instances of 'MonadUnliftIO', but for any base monad.
class MonadBaseControl b m => MonadBaseControlIdentity b m | m -> b where
  liftBaseWithIdentity :: ((RunIdentityInBase m b) -> b a) -> m a

type RunIdentityInBase m b = forall a. m a -> b a

type RunIdentityInBaseDefault t n b = forall a. Monad n => t n a -> b a

defaultLiftBaseWithIdentity :: (MonadBaseControlIdentity b m, MonadTransControlIdentity t)
                            => ((RunIdentityInBaseDefault t m b) -> b a)
                            -> t m a
defaultLiftBaseWithIdentity inner = liftWithIdentity $ \ runId ->
  liftBaseWithIdentity $ \ runIdInBase ->
    inner $ runIdInBase . runId

instance MonadBaseControl b b => MonadBaseControlIdentity b b where
  liftBaseWithIdentity inner = inner id

instance MonadBaseControlIdentity b m => MonadBaseControlIdentity b (IdentityT m) where
  liftBaseWithIdentity = defaultLiftBaseWithIdentity

instance MonadBaseControlIdentity b m => MonadBaseControlIdentity b (ReaderT r m) where
  liftBaseWithIdentity = defaultLiftBaseWithIdentity

class MonadTransControlIdentity t => MonadTransFunctor t where -- TODO: does the superclass here really make sense
  mapT :: (m a -> n b) -> t m a -> t n b

instance MonadTransFunctor IdentityT where
  mapT f = IdentityT . f . runIdentityT

instance MonadTransFunctor (ReaderT r) where
  mapT f m = ReaderT $ f . runReaderT m

liftTrans :: (MonadBaseControl b m, MonadBaseControl b (t m), MonadTransFunctor t)
          => t b a
          -> t m a
liftTrans a = (=<<) restoreM $ liftBaseWith $ \ runInBase ->
                runInBase $ mapT liftBase $ a
