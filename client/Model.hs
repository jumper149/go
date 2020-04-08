module Model ( Model
             , updateModel
             , viewModel
             )where

import Data.Functor.Identity (runIdentity)
import Data.Maybe (isNothing)
import GHC.Generics
import Miso as Miso
import Miso.String
import Text.Read (readMaybe)

import qualified Go.Board.Default as D
import qualified Go.Config as G
import qualified Go.Game.Playing as G
import qualified Go.Game.State as G

import Action
import Board.Default

data Model = Model { gamestate :: G.GameState (D.BoardSquare 2) D.Coord 2
                   , coord     :: Maybe D.Coord
                   }
  deriving (Eq, Ord, Generic, Read, Show)

instance G.Default Model where
  def = Model { gamestate = either undefined id $ runIdentity $ G.runConfiguredT G.def G.initState
              , coord = Nothing
              }

updateModel :: Action -> Model -> Effect Action Model
updateModel action m =
  case action of
    NoOp -> noEff m
    UpdateCoord mstr -> noEff $ m { coord = readMaybe . fromMisoString $ mstr }
    SubmitPlace -> case coord m of
                     Nothing -> undefined
                     Just c -> noEff $ m { gamestate = G.doTurn G.def (G.Place c) (gamestate m)
                                         , coord = Nothing
                                         }
    SubmitPass -> noEff $ m { gamestate = G.doTurn G.def G.Pass (gamestate m)
                            , coord = Nothing
                            }

viewModel :: Model -> View Action
viewModel x =
  div_ [
       ] [ viewBoard (G.currentBoard $ gamestate x) (coord x)
         , div_ [ class_ "control"
                ] [ input_ [ type_ "text"
                           , autofocus_ True
                           , onInput UpdateCoord
                           ]
                  , button_ [ disabled_ (isNothing (coord x))
                            , onClick SubmitPlace
                            ] [ text "Place" ]
                  , button_ [ onClick SubmitPass
                            ] [ text "Pass" ]
                  ]
         ]
