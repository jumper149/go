module Go.Message ( ServerMessageRep (..)
                  , ClientMessageRep (..)
                  ) where

import Data.Aeson
import GHC.Generics

import Go.Config
import Go.Game
import Go.Player

data ServerMessageRep = ServerMessageGameStateRep GameStateRep
                      | ServerMessagePlayerRep (Maybe PlayerRep)
                      | ServerMessageRepLobby [String]
                      | ServerMessageRepFail String
  deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON ServerMessageRep
instance ToJSON ServerMessageRep

data ClientMessageRep = ClientMessageActionRep ActionRep
                      | ClientMessagePlayerRep (Maybe PlayerRep)
                      | ClientMessageRepCreateGame Config
                      | ClientMessageRepFail String
  deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON ClientMessageRep
instance ToJSON ClientMessageRep
