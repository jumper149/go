module Main where

import Prelude
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import Site as S

import Effect.Console (log)

main :: Effect Unit
main = log "hi" *>
    HA.runHalogenAff do
  body <- HA.awaitBody
  runUI B.component unit body
