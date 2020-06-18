{-# LANGUAGE FlexibleContexts #-}

module Network.Wai.Handler.WebSockets.Trans where

-- TODO: credit wai-transformers: https://github.com/athanclark/wai-transformers

import Control.Monad.Base
import Control.Monad.Trans.Control.Identity
import Network.Wai.Handler.WebSockets
import Network.WebSockets

import Network.Wai.Trans

type ServerAppT m = PendingConnection -> m ()

liftServerApp :: MonadBase IO m
              => ServerApp
              -> ServerAppT m
liftServerApp server = liftBase . server

runServerAppT :: MonadBaseControlIdentity IO m
              => ServerAppT m
              -> m ServerApp
runServerAppT server = liftBaseWithIdentity $ \ run ->
  return $ \ pending -> run $ server pending

type ClientAppT m a = Connection -> m a

liftClientApp :: MonadBase IO m
              => ClientApp a
              -> ClientAppT m a
liftClientApp clientApp = liftBase . clientApp

runClientAppT :: MonadBaseControlIdentity IO m
              => ClientAppT m a
              -> m (ClientApp a)
runClientAppT clientApp = liftBaseWithIdentity $ \ run ->
  return $ run . clientApp

websocketsOrT :: MonadBaseControlIdentity IO m
              => ConnectionOptions
              -> ServerAppT m
              -> MiddlewareT m
websocketsOrT options server app request respond = do
  server' <- runServerAppT server
  app' <- runApplicationT app
  liftApplication (websocketsOr options server' app') request respond
