{-# LANGUAGE UndecidableInstances #-}

module Model ( Model
             , updateModel
             , viewModel
             ) where

import Data.Default.Class
import GHC.Generics
import Miso.Effect
import Miso.Html
import Miso.String (ms)

import qualified Go.Player as G
import qualified Go.Game as G
import qualified Go.GameId as G

import Game.Model
import Game.Operation
import Operation
import Representation.Model
import Representation.Operation

data Model = GameM GameModelRep
           | LobbyM [G.GameId]
  deriving (Eq, Generic, Ord, Read, Show)

instance Default Model where
  def = LobbyM mempty

updateModel :: Operation -> Model -> Effect Operation Model
updateModel operation model = case operation of
                                NoOp -> noEff model
                                QueueOp ops -> foldl (\ m a -> updateModel a =<< m) (noEff model) ops
                                GameOp op' -> case model of
                                                GameM m' -> case (m',op') of
                                                              (GameModelD_9_2  m , GameOperationD_9_2  op) -> mapEffect (GameOp . GameOperationD_9_2 ) (GameM . GameModelD_9_2 ) $ updateGameModel op m
                                                              (GameModelD_13_2 m , GameOperationD_13_2 op) -> mapEffect (GameOp . GameOperationD_13_2) (GameM . GameModelD_13_2) $ updateGameModel op m
                                                              _ -> undefined
                                                _ -> undefined -- TODO: just return safely?
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
                                                                                     _ -> undefined

                                GameSetStateRep gs' -> case model of
                                                        GameM m' -> case (m',gs') of
                                                                      (GameModelD_9_2  m , G.GameStateD_9_2  gs) -> mapEffect (GameOp . GameOperationD_9_2 ) (GameM . GameModelD_9_2 ) $ updateGameModel (SetState gs) m
                                                                      (GameModelD_13_2 m , G.GameStateD_13_2 gs) -> mapEffect (GameOp . GameOperationD_13_2) (GameM . GameModelD_13_2) $ updateGameModel (SetState gs) m

                                LobbyOp op -> case model of
                                                LobbyM m -> case op of -- TODO: outsource into new module
                                                              UpdateGames gs -> noEff $ LobbyM gs
                                WriteErrorLog msg -> undefined -- TODO --noEff $ model { errorLog = errorLog model <> msg }

viewModel :: Model -> View Operation
viewModel (GameM model) = case model of
                            GameModelD_9_2  m -> fmap (GameOp . GameOperationD_9_2 ) $ viewGameModel m
                            GameModelD_13_2 m -> fmap (GameOp . GameOperationD_13_2) $ viewGameModel m
viewModel (LobbyM availableGames) =
  div_ [
       ] [ viewGames $ show <$> availableGames
         ]

viewErrorLog :: String -> View a
viewErrorLog errLog = p_ [] [ text $ ms errLog ]

viewGames :: [String] -> View a
viewGames gs = p_ [] [ text $ ms $ unlines gs ]

mapEffect :: (a1 -> a2)
          -> (m1 -> m2)
          -> Effect a1 m1
          -> Effect a2 m2
mapEffect fa fm (Effect model subs) = Effect (fm model) $ map (mapSub fa) subs
