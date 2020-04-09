module Model ( Model
             , updateModel
             , viewModel
             ) where

import Data.Default.Class
import Data.Functor.Identity (runIdentity)
import GHC.Generics
import Miso.Effect
import Miso.Html

import qualified Go.Config as G
import qualified Go.Game.Game as G
import qualified Go.Game.Playing as G
import qualified Go.Game.State as G

import Game
import Operation

data Model b c n = Model { gamestate  :: G.GameState b c n
                         , gameAction :: Maybe (G.Action c)
                         }
  deriving (Eq, Ord, Generic, Read, Show)

instance G.Game b c n => Default (Model b c n) where
  def = Model { gamestate = either undefined id $ runIdentity $ G.runConfiguredT def G.initState
              , gameAction = Nothing
              }

updateModel :: G.Game b c n => Operation c -> Model b c n -> Effect (Operation c) (Model b c n)
updateModel action model =
  case action of
    NoOp -> noEff model
    QueueOp as -> foldr (\ a m -> updateModel a =<< m) (return model) as
    UpdateAction mbAct -> noEff $ model { gameAction = mbAct }
    SubmitAction -> case gameAction model of
                      Nothing -> noEff $ model { gameAction = Nothing } -- TODO: weird exception catch? Prevented by clever button.
                      Just a -> noEff $ model { gamestate = G.doTurn def a (gamestate model)
                                              , gameAction = Nothing
                                              }

viewModel :: MisoGame b c n => Model b c n -> View (Operation c)
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
