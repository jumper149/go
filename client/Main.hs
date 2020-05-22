{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Default.Class
import Miso

import qualified Board.Default as D
import Operation
import Model

main :: IO ()
main = startApp App {..}
  where
    initialAction = NoOp
    model  = def :: Model (D.Board 19 2) (D.Coord 19) 2
    update = updateModel
    view   = viewModel
    events = defaultEvents
    subs   = [ sseSub "/sse" handleSSE
             --, websocketSub (URL "/wss") (Protocols []) (handleWS :: WebSocket String -> Operation (D.Board 19 2) (D.Coord 19) 2)
             ]
    mountPoint = Nothing
