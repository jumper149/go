{-# LANGUAGE FlexibleContexts, OverloadedStrings, RankNTypes, RecordWildCards #-}

module Server where

import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Monad.Trans.Control.Identity
import Data.Default.Class
import Data.Proxy
import GHC.Conc
import Network.Wai.Handler.Warp (Port, run)
import Servant
import System.Directory (listDirectory)

import API
import Handler
import ServerState

import Go.Game.Config
import Go.Game.End
import Go.Game.State
import Go.Run.JSON

server :: forall b c n a. JSONGame b c n => Port -> FilePath -> Proxy b -> IO ()
server port path _ = do putStrLn $ "Port is: " <> show port
                        putStrLn . ("Public files are: " <>) . unwords =<< listDirectory path

                        fmap fst $ runNewServerStateT def $ do
                          hoistedServer <- hoistServerTrans api $ handler path
                          liftBase $ run port $ serve api hoistedServer :: ServerStateT b c n IO ()

hoistServerTrans :: forall api t. (HasServer api '[], MonadTransFunctor t)
              => Proxy api
              -> ServerT api (t Handler)
              -> t IO (ServerT api Handler)
hoistServerTrans a st = liftWithIdentity $ \ runId ->
    return $ hoistServer' $ \ th ->
        (=<<) restoreM $ liftBaseWith $ \ runInBase ->
            runId . mapT runInBase $ th
  where hoistServer' :: (forall a. t Handler a -> Handler a) -> ServerT api Handler
        hoistServer' hoist = hoistServer a hoist st
