{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Default.Class
import Miso

import qualified Board.Default as D
import Operation
import Model

import qualified Go.Run.JSON as G

main :: IO ()
main = startApp App {..}
  where
    initialAction = NoOp
    model  = def :: Model (D.Board 19 2) (D.Coord 19) 2
    update = updateModel
    view   = viewModel
    events = defaultEvents
    subs   = [ websocketSub (URL "ws://local.felixspringer.xyz:8022/wss") (Protocols []) (handleWS :: WebSocket (G.ServerMessage (D.Board 19 2) (D.Coord 19) 2) -> Operation (D.Board 19 2) (D.Coord 19) 2)
             ]
    mountPoint = Nothing
