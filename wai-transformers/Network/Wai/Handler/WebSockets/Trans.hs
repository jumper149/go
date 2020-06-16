{-# LANGUAGE FlexibleContexts #-}
module Network.Wai.Handler.WebSockets.Trans where

-- TODO: credit wai-transformers

import Network.WebSockets (ConnectionOptions, ClientApp, ServerApp, Connection, PendingConnection)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.Wai.Trans (MiddlewareT, runApplicationT, liftApplication)
import Data.Singleton.Class (Extractable (runSingleton))
import Control.Monad.IO.Class (MonadIO (liftIO)) -- TODO: use MonadBase IO
import Control.Monad.Trans.Control (MonadBaseControl (liftBaseWith))

type ServerAppT m = PendingConnection -> m ()

liftServerApp :: MonadIO m
              => ServerApp -- ^ To lift
              -> ServerAppT m
liftServerApp s = liftIO . s

runServerAppT :: MonadBaseControl IO m stM
              => Extractable stM
              => ServerAppT m -- ^ To run
              -> m ServerApp
runServerAppT s = liftBaseWith $ \runInBase ->
  pure $ \pending -> runSingleton <$> runInBase (s pending)

 type ClientAppT m a = Connection -> m a

liftClientApp :: MonadIO m
              => ClientApp a -- ^ To lift
              -> ClientAppT m a
liftClientApp c = liftIO . c

runClientAppT :: MonadBaseControl IO m stM
              => Extractable stM
              => ClientAppT m a -- ^ To run
              -> m (ClientApp a)
runClientAppT c = liftBaseWith $ \runInBase ->
  pure $ \conn -> runSingleton <$> runInBase (c conn)


 -- * WAI Compatability

 -- | Respond with the WebSocket server when applicable, as a middleware
 websocketsOrT :: MonadBaseControl IO m stM
               => Extractable stM
               => ConnectionOptions
               -> ServerAppT m -- ^ Server
               -> MiddlewareT m
websocketsOrT cOpts server app req respond = do
  server' <- runServerAppT server
  app' <- runApplicationT app
  liftApplication (websocketsOr cOpts server' app') req respond
