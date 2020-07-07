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
liftApplication app request respond = liftBaseWithIdentity $ \ run ->
  app request $ \ response -> run $ respond response -- TODO simpler

liftMiddleware :: MonadBaseControlIdentity IO m
               => Middleware
               -> MiddlewareT m
liftMiddleware mid app request respond = do
  app' <- runApplicationT app
  liftBaseWithIdentity $ \ run -> mid app' request $ run . respond

runApplicationT :: MonadBaseControlIdentity IO m
                => ApplicationT m
                -> m Application
runApplicationT app = liftBaseWithIdentity $ \ run ->
  return $ \ request respond -> run $ app request $ liftBase . respond

runMiddlewareT :: MonadBaseControlIdentity IO m
               => MiddlewareT m
               -> m Middleware
runMiddlewareT mid = liftBaseWithIdentity $ \ run ->
  return $ \ app request respond -> do
    app' <- run $ runApplicationT $ mid $ liftApplication app
    app' request respond
