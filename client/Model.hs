module Model ( Model
             , updateModel
             , viewModel
             ) where

import Data.Default.Class
import GHC.Generics
import Miso.Effect
import Miso.Html
import Miso.String (ms)

import qualified Go.Game.Game as G
import qualified Go.Run.JSON as G

import Game.Model
import Game.Run
import Operation

data Model b c n = GameM (GameModel b c n)
                 | Lobby { availableGames :: String
                         }
  deriving (Eq, Ord, Generic, Read, Show)

instance G.Game b c n => Default (Model b c n) where
  def = GameM def

updateModel :: forall b c n. G.JSONGame b c n => Operation b c n -> Model b c n -> Effect (Operation b c n) (Model b c n)
updateModel operation model = case operation of
                                NoOp -> noEff model
                                QueueOp ops -> foldl (\ m a -> updateModel a =<< m) (noEff model) ops
                                GameOp op -> case model of
                                               GameM m -> mapEffect id GameM $ updateGameModel op m
                                               _ -> undefined -- TODO: just return safely?
                                LobbyOp op -> case op of
                                             AskAvailableGames -> model <# undefined
                                             JoinGame _ -> model <# undefined
                                WriteErrorLog msg -> undefined -- TODO --noEff $ model { errorLog = errorLog model <> msg }

viewModel :: MisoGame b c n => Model b c n -> View (Operation b c n)
viewModel (GameM model) = fmap GameOp $ viewGameModel model
viewModel Lobby { availableGames = availableGames } =
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
