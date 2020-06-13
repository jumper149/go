{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Server where

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

server :: forall b c n. JSONGame b c n => Port -> FilePath -> Proxy b -> IO ()
server port path _ = do putStrLn $ "Port is: " <> show port
                        putStrLn . ("Public files are: " <>) . unwords =<< listDirectory path

                        let hoistHandler = fmap fst . runNewServerStateT config
                            app = serve api $ hoistServer api hoistHandler (handler path :: ServerT API (ServerStateHandler b c n))
                        run port app
  where config = def
