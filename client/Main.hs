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
    model  = def :: Model (D.BoardSquare 2) D.Coord 2
    update = updateModel
    view   = viewModel
    events = defaultEvents
    subs   = []
    mountPoint = Nothing
