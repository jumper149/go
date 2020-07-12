{-# LANGUAGE RecordWildCards #-}

module Main ( main
            ) where

import Data.Default.Class
import Data.Proxy
import qualified Data.Text as T
import Miso
import Miso.String (ms)
import Servant.API

import Go.Run.API

import Message
import Model
import Operation

main :: JSM ()
main = do currentURI <- getCurrentURI
          let initialAction  = let eithGameId = parseUrlPiece . T.pack . stripLeadingSlash $ uriPath currentURI
                               in case eithGameId of
                                    Right gameId -> SetAwaitGame gameId
                                    Left _ -> NoOp
              model  = LobbyM def
              update = updateModel
              view   = viewModel
              events = defaultEvents
              subs   = let uri = URI { uriScheme = "ws:"
                                     , uriAuthority = uriAuthority currentURI
                                     , uriPath = T.unpack $ ("/" <>) $ toUrlPiece $ safeLink apiWrongWS (Proxy :: Proxy EndpointWSWrongWS)
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

stripLeadingSlash :: String -> String
stripLeadingSlash ('/':str) = str
stripLeadingSlash str = str
