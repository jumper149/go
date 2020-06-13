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

import Go.Game.Config
import Go.Game.End
import Go.Game.State
import Go.Run.JSON

server :: forall b c n. JSONGame b c n => Port -> FilePath -> Proxy (EndScreen b n) -> IO ()
server port path _ = do putStrLn $ "Port is: " <> show port
                        putStrLn . ("Public files are: " <>) . unwords =<< listDirectory path

                        initial <- either (error . show) id <$> runConfiguredT config initState :: IO (GameState b c n) -- TODO: error?
                        gameStateTVar <- newTVarIO initial
                        clientsTVar <- newTVarIO mempty

                        let gameConfig = def
                            app = serve api $ hoistServer api (runHandlerM ServerState {..}) (handler path :: ServerT API (HandlerM b c n))
                        run port app
                        return undefined -- TODO: undefined behaviour
  where config = def
