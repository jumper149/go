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
liftServerApp serverApp = liftBase . serverApp

runServerAppT :: MonadBaseControlIdentity IO m
              => ServerAppT m
              -> m ServerApp
runServerAppT serverAppT = liftBaseWithIdentity $ \ runInBase ->
  return $ runInBase . serverAppT

type ClientAppT m a = Connection -> m a

liftClientApp :: MonadBase IO m
              => ClientApp a
              -> ClientAppT m a
liftClientApp clientApp = liftBase . clientApp

runClientAppT :: MonadBaseControlIdentity IO m
              => ClientAppT m a
              -> m (ClientApp a)
runClientAppT clientAppT = liftBaseWithIdentity $ \ runInBase ->
  return $ runInBase . clientAppT

websocketsOrT :: MonadBaseControlIdentity IO m
              => ConnectionOptions
              -> ServerAppT m
              -> MiddlewareT m
websocketsOrT options serverAppT appT request respond = do
  serverApp <- runServerAppT serverAppT
  app <- runApplicationT appT
  (liftApplication $ websocketsOr options serverApp app) request respond
