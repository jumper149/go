{-# LANGUAGE RecordWildCards, TypeApplications, UndecidableInstances #-}

module Game.Model ( GameModel (..)
                  , updateGameModel
                  , viewGameModel
                  ) where

import Data.Default.Class
import GHC.Generics
import GHC.TypeLits
import Miso.Effect
import Miso.Html
import Miso.Subscription.WebSocket

import qualified Go.Game.Game as G
import qualified Go.Game.State as G
import qualified Go.Message as G
import qualified Go.Representation as G
import qualified Go.Run.JSON as G

import Game.Operation
import Game.Run
import Game.Svg
import Operation

data GameModel b = GameModel { gameState :: G.AssociatedGameState b
                             , gameAction :: Maybe (G.AssociatedAction b)
                             , chosenPlayer :: Maybe (G.AssociatedPlayer b)
                             , errorLog :: String
                             }
  deriving Generic
deriving instance (Eq b, Eq (G.AssociatedCoord b)) => Eq (GameModel b)
deriving instance (Ord b, Ord (G.AssociatedCoord b)) => Ord (GameModel b)
deriving instance (Read b, Read (G.AssociatedCoord b), KnownNat (G.AssociatedPlayerCount b)) => Read (GameModel b)
deriving instance (Show b, Show (G.AssociatedCoord b)) => Show (GameModel b)

instance G.Game b => Default (GameModel b) where
  def = GameModel { gameState = G.initState
                  , gameAction = Nothing
                  , chosenPlayer = Nothing
                  , errorLog = mempty
                  }

updateGameModel :: forall b. (G.JSONGame b, G.RepresentableGame b)
                => GameOperation b
                -> GameModel b
                -> Effect (Operation b) (GameModel b)
updateGameModel operation GameModel {..} =
  case operation of
    UpdateAction gameAction -> noEff GameModel {..}
    SubmitAction -> case gameAction of
                      Nothing -> noEff GameModel {..} -- TODO: weird exception catch? Prevented by clever button.
                      Just a -> GameModel {..} <# do send $ G.ClientMessageActionRep $ (G.toActionRep @b) a
                                                     return NoOp
    SetState gameState -> noEff $ GameModel { gameAction = Nothing, ..}
    SubmitPlayer mbP -> GameModel {..} <# do send $ G.ClientMessagePlayerRep $ (G.toPlayerRep @b) <$> mbP
                                             return NoOp
    SetPlayer chosenPlayer -> noEff $ GameModel {..}

viewGameModel :: MisoGame b
              => GameModel b
              -> View (GameOperation b)
viewGameModel GameModel {..} =
  div_ [
       ] [ viewBoard (G.currentBoard gameState) coord
         , viewPassButton gameAction
         , viewPlayerChoice chosenPlayer
         ]
  where coord = case gameAction of
                  Just (G.Place c) -> Just c
                  _ -> Nothing
