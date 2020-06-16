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

--type ClientAppT m a = Connection -> m a
--
--liftClientApp :: MonadIO m
--              => ClientApp a -- ^ To lift
--              -> ClientAppT m a
--liftClientApp c = liftIO . c
--
--runClientAppT :: MonadBaseControl IO m stM
--              => Extractable stM
--              => ClientAppT m a -- ^ To run
--              -> m (ClientApp a)
--runClientAppT c = liftBaseWith $ \runInBase ->
--  pure $ \conn -> runSingleton <$> runInBase (c conn)

websocketsOrT :: MonadBaseControlIdentity IO m
              => ConnectionOptions
              -> ServerAppT m -- ^ Server
              -> MiddlewareT m
websocketsOrT options server app request respond = do
  server' <- runServerAppT server
  app' <- runApplicationT app
  liftApplication (websocketsOr options server' app') request respond
