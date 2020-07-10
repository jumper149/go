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
                      | ServerMessageLobby [Integer]
                      | ServerMessageRepFail String
  deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON ServerMessageRep
instance ToJSON ServerMessageRep

data ClientMessageRep = ClientMessageActionRep ActionRep
                      | ClientMessagePlayerRep (Maybe PlayerRep)
                      | ClientMessageCreateGame Config
                      | ClientMessageRepFail String
  deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON ClientMessageRep
instance ToJSON ClientMessageRep
