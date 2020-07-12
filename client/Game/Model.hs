{-# LANGUAGE UndecidableInstances #-}

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
                -> Effect (GameOperation b) (GameModel b)
updateGameModel operation model =
  case operation of
    GameNoOp -> noEff model
    UpdateAction a -> noEff model { gameAction = a }
    SubmitAction -> case gameAction model of
                      Nothing -> noEff model -- TODO: weird exception catch? Prevented by clever button.
                      Just a -> model <# do send $ G.ClientMessageActionRep $ (G.toActionRep @b) a
                                            return GameNoOp
    SetState gs -> noEff $ model { gameAction = Nothing, gameState = gs }
    SubmitPlayer mbP -> model <# do send $ G.ClientMessagePlayerRep $ (G.toPlayerRep @b) <$> mbP
                                    return GameNoOp
    SetPlayer p -> noEff $ model { chosenPlayer = p }

viewGameModel :: MisoGame b
              => GameModel b
              -> View (GameOperation b)
viewGameModel GameModel { gameState = gs, gameAction = a, chosenPlayer = p } =
  div_ [
       ] [ viewBoard (G.currentBoard gs) coord
         , viewPassButton a
         , viewPlayerChoice p
         ]
  where coord = case a of
                  Just (G.Place c) -> Just c
                  _ -> Nothing
