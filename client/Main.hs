{-# LANGUAGE RecordWildCards #-}

module Main where

import Miso

import Go.Config (Default (..))

import Action
import Model

main :: IO ()
main = startApp App {..}
  where
    initialAction = NoOp
    model  = def
    update = updateModel
    view   = viewModel
    events = defaultEvents
    subs   = []
    mountPoint = Nothing
