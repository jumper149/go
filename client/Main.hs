{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main where

import Data.Functor.Identity
import GHC.Generics
import Miso
import Miso.String

import qualified Go.Board.Default as D
import qualified Go.Config as G
import qualified Go.Game.Playing as G
import qualified Go.Game.Rules as G
import qualified Go.Game.State as G

main :: IO ()
main = startApp App {..}
  where
    initialAction = NoOp
    model  = either undefined id $ runIdentity $ G.runConfiguredT G.def G.initState
    update = updateModel
    view   = viewModel
    events = defaultEvents
    subs   = []
    mountPoint = Nothing

type Model = G.GameState (D.BoardSquare 2) D.Coord 2

data Action = NoOp
            | Act (G.Action D.Coord)
  deriving (Eq, Ord, Generic, Read, Show)

updateModel :: Action -> Model -> Effect Action Model
updateModel action m =
  case action of
    Act a
      -> noEff $ G.doTurn G.def a m
    NoOp
      -> noEff m

viewModel :: Model -> View Action
viewModel x = div_ [] [
   button_ [ onClick (Act (G.Place (D.Coord 2 2))) ] [ text "2 2" ]
 , text (ms (show x))
 ]
