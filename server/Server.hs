{-# LANGUAGE FlexibleContexts, KindSignatures, OverloadedStrings, RecordWildCards #-}

module Server where

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.Binary.Builder as B
import qualified Data.ByteString.Lazy as BS
import Data.Default.Class
import qualified Data.Text as T
import GHC.Generics
import Network.Wai.Handler.Warp (Port, run)
import Servant
import System.Directory (listDirectory)

import API
import Handler
import Html
import Message

import Go.Game.Config
import Go.Game.End
import Go.Game.State
import Go.Run.JSON

server :: forall b c n. JSONGame b c n => Port -> FilePath -> IO (EndScreen b n)
server port path = do putStrLn $ "Port is: " <> show port
                      putStrLn . ("Public files are: " <>) . unwords =<< listDirectory path

                      initial <- either (error . show) id <$> runConfiguredT config initState :: IO (GameState b c n) -- TODO: error?
                      gameStateMVar <- newMVar initial

                      let gameConfig = def
                          app = serve api $ hoistServer api (runHandlerM ServerState {..}) (handler path)
                      run port app
                      return undefined -- TODO: undefined behaviour
  where config = def
