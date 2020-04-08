module Model ( Model
             , updateModel
             , viewModel
             )where

import Data.Functor.Identity (runIdentity)
import Data.Maybe (isNothing)
import GHC.Generics
import GHC.TypeLits
import Miso as Miso
import Miso.String
import Text.Read (readMaybe)

import qualified Go.Board.Default as D
import qualified Go.Config as G
import qualified Go.Game.Playing as G
import qualified Go.Game.State as G

import Action
import Board.Default

data Model n = Model { gamestate :: G.GameState (D.BoardSquare n) D.Coord n
                     , coord     :: Maybe D.Coord
                     }
  deriving (Eq, Ord, Generic, Read, Show)

instance KnownNat n => G.Default (Model n) where
  def = Model { gamestate = either undefined id $ runIdentity $ G.runConfiguredT G.def G.initState
              , coord = Nothing
              }

updateModel :: KnownNat n => Action -> Model n -> Effect Action (Model n)
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

viewModel :: KnownNat n => Model n -> View Action
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
