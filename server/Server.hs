{-# LANGUAGE OverloadedStrings, RankNTypes, RecordWildCards #-}

module Server where

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
                          (hoistRun $ \ x -> run port $ serve api $ hoistServer api x $ handler path) :: ServerStateT b c n IO ()

hoistRun :: ((forall a. ServerStateT b c n Handler a -> Handler a) -> IO ()) -> ServerStateT b c n IO ()
hoistRun runToHoist = liftBaseWithIdentity $ \ runInBaseId ->
    runToHoist $ \ ssTH ->
        (=<<) restoreM $ liftBaseWith $ \ runInBase ->
            runInBaseId . mapT runInBase $ ssTH
