{-# LANGUAGE RecordWildCards #-}

module Main ( main
            ) where

import Data.Default.Class
import Miso

import Message
import Model
import Operation

main :: JSM ()
main = do currentURI <- getCurrentURI
          let initialAction = if uriPath currentURI == ""
                                 then NoOp
                                 else AwaitGame
              model  = LobbyM def
              update = updateModel
              view   = viewModel
              events = defaultEvents
              subs   = let url = URL "ws://local.felixspringer.xyz:8022/ws"
                           protocols = Protocols []
                           handler = handleWS
                       in [ websocketSub url protocols handler ]
              mountPoint = Nothing
              logLevel = Off
          startApp App {..}
