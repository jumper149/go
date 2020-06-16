{-# LANGUAGE FlexibleContexts , Rank2Types #-}

-- TODO: credit wai-transformers

module Network.Wai.Trans where

import Control.Monad.Trans.Control
import Control.Monad.Trans.Control.Isomorphic
import Network.Wai (Application, Middleware, Request, Response, ResponseReceived)

-- | Isomorphic to @Kleisli (ContT ResponseReceived m) Request Response@
type ApplicationT m = Request -> (Response -> m ResponseReceived) -> m ResponseReceived
type MiddlewareT m = ApplicationT m -> ApplicationT m

liftApplication :: MonadBaseControlIsomorphic IO m
                => Application -- ^ To lift
                -> ApplicationT m
liftApplication app request respond = withRunInBase $ \ run ->
  app request $ \ response -> run $ respond response

liftMiddleware :: MonadBaseControl IO m stM
               => Extractable stM
               => Middleware -- ^ To lift
               -> MiddlewareT m
liftMiddleware mid app req respond = do
  app' <- runApplicationT app
  liftBaseWith (\runInBase -> mid app' req (fmap runSingleton . runInBase . respond))

--runApplicationT :: MonadBaseControl IO m stM
--                => Extractable stM
--                => ApplicationT m -- ^ To run
--                -> m Application
--runApplicationT app = liftBaseWith $ \runInBase ->
--  pure $ \req respond -> fmap runSingleton $ runInBase $ app req (\x -> liftBaseWith (\_ -> respond x))
--
--runMiddlewareT :: MonadBaseControl IO m stM
--               => Extractable stM
--               => MiddlewareT m -- ^ To run
--               -> m Middleware
--runMiddlewareT mid = liftBaseWith $ \runInBase ->
--  pure $ \app req respond -> do
--    app' <- fmap runSingleton $ runInBase $ runApplicationT (mid (liftApplication app))
--    app' req respond
--
--hoistApplicationT :: Monad m
--                  => Monad n
--                  => (forall a. m a -> n a) -- ^ To
--                  -> (forall a. n a -> m a) -- ^ From
--                  -> ApplicationT m
--                  -> ApplicationT n
--hoistApplicationT to from app req resp =
--  to $ app req (from . resp)
--
--hoistMiddlewareT :: Monad m
--                 => Monad n
--                 => (forall a. m a -> n a) -- ^ To
--                 -> (forall a. n a -> m a) -- ^ From
--                 -> MiddlewareT m
--                 -> MiddlewareT n
--hoistMiddlewareT to from mid =
--  hoistApplicationT to from . mid . hoistApplicationT from to
