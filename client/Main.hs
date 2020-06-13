{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Default.Class
import Data.Proxy
import Miso

import qualified Board.Default as D
import Game
import Operation
import Model

import qualified Go.Run.JSON as G

main :: IO ()
main = run (Proxy :: Proxy (D.Board 19 2))

run :: forall b c n. MisoGame b c n => Proxy b -> IO ()
run _ = startApp App {..}
  where
    initialAction = NoOp
    model  = def :: Model b c n
    update = updateModel
    view   = viewModel
    events = defaultEvents
    subs   = let url = URL "ws://local.felixspringer.xyz:8022/wss"
                 protocols = Protocols []
                 handler = handleWS :: WebSocket (G.ServerMessage b c n) -> Operation b c n
             in [ websocketSub url protocols handler ]
    mountPoint = Nothing
