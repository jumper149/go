module Lobby.Model ( LobbyModel (..)
                   , updateLobbyModel
                   , viewLobbyModel
                   ) where

import Data.Default.Class
import GHC.Generics
import Miso.Effect
import Miso.Html
import Miso.String (ms)

import qualified Go.GameId as G

import Lobby.Operation

data LobbyModel = LobbyModel { availableGames :: [G.GameId] }
  deriving (Eq, Generic, Ord, Read, Show)

instance Default LobbyModel where
  def = LobbyModel mempty

updateLobbyModel :: LobbyOperation -> LobbyModel -> Effect LobbyOperation LobbyModel
updateLobbyModel operation model = case operation of
                                     UpdateGames gs -> noEff model { availableGames = gs }

viewLobbyModel :: LobbyModel -> View LobbyOperation
viewLobbyModel LobbyModel { availableGames = gs } =
  div_ [
       ] [ viewGames $ show <$> gs
         ]

viewGames :: [String] -> View a
viewGames gs = p_ [] [ text $ ms $ unlines gs ]
