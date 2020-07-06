{-# LANGUAGE RecordWildCards #-}

module Main ( main
            ) where

import Data.Default.Class
import Miso

import qualified Go.Representation as G

import qualified Game.Board.Default as D
import Game.Run
import Message
import Model
import Operation

type GameApp b = App (Model b) (Operation b)

app :: forall b. (MisoGame b, G.RepresentableGame b) => GameApp b
app = App {..}
  where initialAction = NoOp
        model  = def
        update = updateModel
        view   = viewModel
        events = defaultEvents
        subs   = let url = URL "ws://local.felixspringer.xyz:8022/ws/1"
                     protocols = Protocols []
                     handler = handleWS
                 in [ websocketSub url protocols handler ]
        mountPoint = Nothing
        logLevel = Off

main :: JSM ()
main = do URI {..} <- getCurrentURI
          case uriPath of
            _ -> startApp (app :: GameApp (D.Board 9 2))
