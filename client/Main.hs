{-# LANGUAGE RecordWildCards #-}

module Main where

import Miso

import qualified Go.Board.Default as D
import Go.Config (Default (..))

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
