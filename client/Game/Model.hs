{-# LANGUAGE RecordWildCards #-}

module Game.Model ( GameModel (..)
                  , updateGameModel
                  , viewGameModel
                  ) where

import Data.Default.Class
import GHC.Generics
import Miso.Effect
import Miso.Html
import Miso.Subscription.WebSocket

import qualified Go.Game.Game as G
import qualified Go.Game.Player as G
import qualified Go.Game.State as G
import qualified Go.Run.JSON as G

import Game.Operation
import Game.Run
import Game.Svg
import Operation

data GameModel b c n = GameModel { gameState :: G.GameState b c n
                                 , gameAction :: Maybe (G.Action c)
                                 , chosenPlayer :: Maybe (G.PlayerN n)
                                 , errorLog :: String
                                 }
  deriving (Eq, Ord, Generic, Read, Show)

instance G.Game b c n => Default (GameModel b c n) where
  def = GameModel { gameState = G.initState
                  , gameAction = Nothing
                  , chosenPlayer = Nothing
                  , errorLog = mempty
                  }

updateGameModel :: forall b c n. G.JSONGame b c n
                => GameOperation b c n
                -> GameModel b c n
                -> Effect (Operation b c n) (GameModel b c n)
updateGameModel operation GameModel {..} =
  case operation of
    UpdateAction gameAction -> noEff GameModel {..}
    SubmitAction -> case gameAction of
                      Nothing -> noEff GameModel {..} -- TODO: weird exception catch? Prevented by clever button.
                      Just a -> GameModel {..} <# do send (G.ClientMessageAction a :: G.ClientMessage b c n)
                                                     return NoOp
    SetState gameState -> noEff $ GameModel { gameAction = Nothing, ..}
    SubmitPlayer mbP -> GameModel {..} <# do send (G.ClientMessagePlayer mbP :: G.ClientMessage b c n)
                                             return NoOp
    SetPlayer chosenPlayer -> noEff $ GameModel {..}

viewGameModel :: MisoGame b c n
              => GameModel b c n
              -> View (GameOperation b c n)
viewGameModel GameModel {..} =
  div_ [
       ] [ viewBoard (G.currentBoard gameState) coord
         , viewPassButton gameAction
         , viewPlayerChoice chosenPlayer
         ]
  where coord = case gameAction of
                  Just (G.Place c) -> Just c
                  _ -> Nothing
