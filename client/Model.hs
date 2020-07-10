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

import Game.Model
import Operation
import Representation.Model
import Representation.Operation

data Model = GameM GameModelRep
           | LobbyM String
  deriving (Eq, Generic, Ord, Read, Show)

instance Default Model where
  def = LobbyM ""

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
                                GameSetPlayerRep _ -> undefined
                                GameSetStateRep _ -> undefined
                                LobbyOp op -> case op of
                                             AskAvailableGames -> model <# undefined
                                             JoinGame _ -> model <# undefined
                                WriteErrorLog msg -> undefined -- TODO --noEff $ model { errorLog = errorLog model <> msg }

viewModel :: Model -> View Operation
viewModel (GameM model) = case model of
                            GameModelD_9_2  m -> fmap (GameOp . GameOperationD_9_2 ) $ viewGameModel m
                            GameModelD_13_2 m -> fmap (GameOp . GameOperationD_13_2) $ viewGameModel m
viewModel (LobbyM availableGames) =
  div_ [
       ] [ viewGames availableGames
         ]

viewErrorLog :: String -> View a
viewErrorLog errLog = p_ [] [ text $ ms errLog ]

viewGames :: b -> View a
viewGames _ = p_ [] [ text $ ms ("asdasd" :: String) ]

mapEffect :: (a1 -> a2)
          -> (m1 -> m2)
          -> Effect a1 m1
          -> Effect a2 m2
mapEffect fa fm (Effect model subs) = Effect (fm model) $ map (mapSub fa) subs
