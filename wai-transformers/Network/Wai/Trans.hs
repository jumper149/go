{-# LANGUAGE FlexibleContexts #-}

-- TODO: credit wai-transformers: https://github.com/athanclark/wai-transformers

module Network.Wai.Trans where

import Control.Monad.Base
import Control.Monad.Trans.Control.Identity
import Network.Wai

type ApplicationT m = Request -> (Response -> m ResponseReceived) -> m ResponseReceived
type MiddlewareT m = ApplicationT m -> ApplicationT m

liftApplication :: MonadBaseControlIdentity IO m
                => Application
                -> ApplicationT m
liftApplication app request respond = liftBaseWithIdentity $ \ runInBase ->
  app request $ runInBase . respond

liftMiddleware :: MonadBaseControlIdentity IO m
               => Middleware
               -> MiddlewareT m
liftMiddleware mid appT request respond = do
  app <- runApplicationT appT
  liftBaseWithIdentity $ \ runInBase -> mid app request $ runInBase . respond

runApplicationT :: MonadBaseControlIdentity IO m
                => ApplicationT m
                -> m Application
runApplicationT appT = liftBaseWithIdentity $ \ runInBase ->
  return $ \ request respond -> runInBase $ appT request $ liftBase . respond

runMiddlewareT :: MonadBaseControlIdentity IO m
               => MiddlewareT m
               -> m Middleware
runMiddlewareT midT = liftBaseWithIdentity $ \ runInBase ->
  return $ \ app request respond -> do
    app' <- runInBase . runApplicationT . midT $ liftApplication app
    app' request respond
