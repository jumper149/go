{-# LANGUAGE UndecidableInstances #-}

module Model ( Model (..)
             , updateModel
             , viewModel
             ) where

import Data.Default.Class
import GHC.Generics
import Miso.Effect
import Miso.Html
import Miso.Subscription.WebSocket

import qualified Go.Representation as G
import qualified Go.Run.GameId as G
import qualified Go.Run.Message as G

import AwaitingGame.Html
import Game.Model
import Game.Operation
import Lobby.Model
import Operation
import Representation
import Representation.Model
import Representation.Operation

data Model = GameM GameModelRep
           | LobbyM LobbyModel
           | AwaitingGame G.GameId
  deriving (Eq, Generic, Ord, Read, Show)

updateModel :: Operation -> Model -> Effect Operation Model
updateModel operation model = case operation of
                                NoOp -> noEff model
                                QueueOp ops -> foldl (\ m a -> updateModel a =<< m) (noEff model) ops
                                GameOp op' -> case model of
                                                GameM m' -> case (m',op') of
                                                              (GameModelD_9_2  m , GameOperationD_9_2  op) -> mapEffect (GameOp . GameOperationD_9_2 ) (GameM . GameModelD_9_2 ) $ updateGameModel op m
                                                              (GameModelD_13_2 m , GameOperationD_13_2 op) -> mapEffect (GameOp . GameOperationD_13_2) (GameM . GameModelD_13_2) $ updateGameModel op m
                                                              _ -> noEff model --TODO?
                                                _ -> noEff model --TODO?
                                GameSetPlayerRep mbP' -> case mbP' of
                                                           Nothing -> case model of -- TODO: no typechecking here, have maybe inside of Rep-type?
                                                                        GameM m' -> case m' of
                                                                                      GameModelD_9_2  m -> mapEffect (GameOp . GameOperationD_9_2 ) (GameM . GameModelD_9_2 ) $ updateGameModel (SetPlayer Nothing) m
                                                                                      GameModelD_13_2 m -> mapEffect (GameOp . GameOperationD_13_2) (GameM . GameModelD_13_2) $ updateGameModel (SetPlayer Nothing) m
                                                                        _ -> noEff model
                                                           Just p' -> case model of
                                                                       GameM m' -> case (m',p') of
                                                                                     (GameModelD_9_2  m , G.PlayerD_9_2  p) -> mapEffect (GameOp . GameOperationD_9_2 ) (GameM . GameModelD_9_2 ) $ updateGameModel (SetPlayer (Just p)) m
                                                                                     (GameModelD_13_2 m , G.PlayerD_13_2 p) -> mapEffect (GameOp . GameOperationD_13_2) (GameM . GameModelD_13_2) $ updateGameModel (SetPlayer (Just p)) m
                                                                                     _ -> noEff model --TODO?
                                                                       _ -> noEff model --TODO?
                                GameSetStateRep gs' -> case model of
                                                        GameM m' -> case (m',gs') of
                                                                      (GameModelD_9_2  m , G.GameStateD_9_2  gs) -> mapEffect (GameOp . GameOperationD_9_2 ) (GameM . GameModelD_9_2 ) $ updateGameModel (SetState gs) m
                                                                      (GameModelD_13_2 m , G.GameStateD_13_2 gs) -> mapEffect (GameOp . GameOperationD_13_2) (GameM . GameModelD_13_2) $ updateGameModel (SetState gs) m
                                                                      _ -> noEff model --TODO?
                                                        AwaitingGame _ -> case gs' of
                                                                            G.GameStateD_9_2  gs -> noEff . GameM $ GameModelD_9_2  def { gameState = gs }
                                                                            G.GameStateD_13_2 gs -> noEff . GameM $ GameModelD_13_2 def { gameState = gs }
                                                        _ -> noEff model --TODO?
                                LobbyOp op -> case model of
                                                 LobbyM m -> mapEffect LobbyOp LobbyM $ updateLobbyModel op m
                                                 _ -> noEff model --TODO?
                                SetAwaitGame gameId -> noEff $ AwaitingGame gameId
                                SubmitAwaitGame -> case model of
                                                     AwaitingGame gameId -> model <# do send $ G.ClientMessagePromote gameId
                                                                                        return NoOp
                                                     _ -> noEff model
                                WriteErrorLog _ -> noEff model --TODO?

viewModel :: Model -> View Operation
viewModel (GameM model) = GameOp <$> viewGameModelRep model
viewModel (LobbyM model) = LobbyOp <$> viewLobbyModel model
viewModel (AwaitingGame gameId) = viewAwaitingGame gameId

mapEffect :: (a1 -> a2)
          -> (m1 -> m2)
          -> Effect a1 m1
          -> Effect a2 m2
mapEffect fa fm (Effect model subs) = Effect (fm model) $ map (mapSub fa) subs
