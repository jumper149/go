module Model ( Model
             , updateModel
             , viewModel
             ) where

import Data.Functor.Identity (runIdentity)
import GHC.Generics
import GHC.TypeLits
import Miso.Effect
import Miso.Html

import qualified Go.Board.Default as D
import qualified Go.Config as G
import qualified Go.Game.Playing as G
import qualified Go.Game.State as G

import Action
import Board.Default

data Model n = Model { gamestate  :: G.GameState (D.BoardSquare n) D.Coord n
                     , gameAction :: Maybe (G.Action D.Coord)
                     }
  deriving (Eq, Ord, Generic, Read, Show)

instance KnownNat n => G.Default (Model n) where
  def = Model { gamestate = either undefined id $ runIdentity $ G.runConfiguredT G.def G.initState
              , gameAction = Nothing
              }

updateModel :: KnownNat n => Action -> Model n -> Effect Action (Model n)
updateModel action model =
  case action of
    NoOp -> noEff model
    QueueOp as -> foldr (\ a m -> updateModel a =<< m) (return model) as
    UpdateAction mbAct -> noEff $ model { gameAction = mbAct }
    SubmitAction -> case gameAction model of
                      Nothing -> noEff $ model { gameAction = Nothing } -- TODO: weird exception catch? Prevented by clever button.
                      Just a -> noEff $ model { gamestate = G.doTurn G.def a (gamestate model)
                                              , gameAction = Nothing
                                              }

viewModel :: KnownNat n => Model n -> View Action
viewModel model =
  div_ [
       ] [ viewBoard (G.currentBoard $ gamestate model) coord
         , button_ [ onClick $ QueueOp [ UpdateAction $ Just G.Pass
                                       , SubmitAction
                                       ]
                   ] [ text "Pass" ]
         ]
  where coord = case gameAction model of
                  Just (G.Place c) -> Just c
                  _ -> Nothing
