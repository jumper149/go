module Model ( Model
             , updateModel
             , viewModel
             ) where

import Data.Default.Class
import Data.Functor.Identity (runIdentity)
import GHC.Generics
import Miso.Effect
import Miso.Html

import qualified Go.Game.Config as G
import qualified Go.Game.Game as G
import qualified Go.Game.Playing as G
import qualified Go.Game.State as G

import Game
import Operation
import Svg

data Model b c n = Model { gameState  :: G.GameState b c n
                         , gameAction :: Maybe (G.Action c)
                         }
  deriving (Eq, Ord, Generic, Read, Show)

instance G.Game b c n => Default (Model b c n) where
  def = Model { gameState = either undefined id $ runIdentity $ G.runConfiguredT def G.initState
              , gameAction = Nothing
              }

updateModel :: G.Game b c n => Operation b c n -> Model b c n -> Effect (Operation b c n) (Model b c n)
updateModel action model =
  case action of
    NoOp -> noEff model
    QueueOp as -> foldl (\ m a -> updateModel a =<< m) (noEff model) as
    UpdateAction mbAct -> noEff $ model { gameAction = mbAct }
    SubmitAction -> case gameAction model of
                      Nothing -> noEff $ model { gameAction = Nothing } -- TODO: weird exception catch? Prevented by clever button.
                      Just a -> noEff $ model { gameState = G.doTurn def a (gameState model)
                                              , gameAction = Nothing
                                              }
    SetState gs -> noEff $ model { gameState = gs
                                 , gameAction = Nothing
                                 }

viewModel :: MisoGame b c n => Model b c n -> View (Operation b c n)
viewModel model =
  div_ [
       ] [ viewBoard (G.currentBoard $ gameState model) coord
         , viewPassButton $ gameAction model
         ]
  where coord = case gameAction model of
                  Just (G.Place c) -> Just c
                  _ -> Nothing
