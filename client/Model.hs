{-# LANGUAGE UndecidableInstances #-}

module Model ( Model
             , updateModel
             , viewModel
             ) where

import Data.Default.Class
import GHC.Generics
import GHC.TypeLits
import Miso.Effect
import Miso.Html
import Miso.String (ms)

import qualified Go.Game.Game as G
import qualified Go.Representation as G
import qualified Go.Run.JSON as G

import Game.Model
import Game.Run
import Operation

data Model b = GameM (GameModel b)
             | Lobby { availableGames :: String
                     }
  deriving Generic
deriving instance (Eq b, Eq (G.AssociatedCoord b)) => Eq (Model b)
deriving instance (Ord b, Ord (G.AssociatedCoord b)) => Ord (Model b)
deriving instance (Read b, Read (G.AssociatedCoord b), KnownNat (G.AssociatedPlayerCount b)) => Read (Model b)
deriving instance (Show b, Show (G.AssociatedCoord b)) => Show (Model b)

instance G.Game b => Default (Model b) where
  def = Lobby def

updateModel :: forall b. (G.JSONGame b, G.RepresentableGame b) => Operation b -> Model b -> Effect (Operation b) (Model b)
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

viewModel :: MisoGame b => Model b -> View (Operation b)
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
