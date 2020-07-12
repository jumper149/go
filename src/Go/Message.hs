module Go.Message ( ServerMessage (..)
                  , ClientMessage (..)
                  ) where

import Data.Aeson
import GHC.Generics

import Go.Config
import Go.Representation.Game
import Go.Representation.Player
import Go.Run.GameId

data ServerMessage = ServerMessageGameStateRep GameStateRep
                   | ServerMessagePlayerRep (Maybe PlayerRep)
                   | ServerMessageLobby [GameId]
                   | ServerMessageApproveConfig Config
                   | ServerMessageFail String
  deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON ServerMessage
instance ToJSON ServerMessage

data ClientMessage = ClientMessageActionRep ActionRep
                   | ClientMessagePlayerRep (Maybe PlayerRep)
                   | ClientMessageCreateGame Config
                   | ClientMessageTryConfig Config
                   | ClientMessagePromote GameId
                   | ClientMessageFail String
  deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON ClientMessage
instance ToJSON ClientMessage
