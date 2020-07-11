{-# LANGUAGE RecordWildCards #-}

module Main ( main
            ) where

import Data.Default.Class
import Miso
import Miso.String (ms)

import Message
import Model
import Operation

main :: JSM ()
main = do currentURI <- getCurrentURI
          let initialAction  = case uriPath currentURI of
                                 "/0" -> AwaitGame $ toEnum 0
                                 "/1" -> AwaitGame $ toEnum 1
                                 _ -> NoOp
              model  = LobbyM def
              update = updateModel
              view   = viewModel
              events = defaultEvents
              subs   = let uri = URI { uriScheme = "ws:"
                                     , uriAuthority = uriAuthority currentURI
                                     , uriPath = "/ws"
                                     , uriQuery = mempty
                                     , uriFragment = mempty
                                     }
                           url = URL . ms $ show uri
                           protocols = Protocols []
                           handler = handleWS
                       in [ websocketSub url protocols handler ]
              mountPoint = Nothing
              logLevel = Off
          startApp App {..}
