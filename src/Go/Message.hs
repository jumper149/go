module Go.Message ( ServerMessageRep (..) -- TODO: remove
                  , ClientMessageRep (..) -- TODO: remove
                  , ServerMessage (..)
                  , ClientMessage (..)
                  ) where

import Data.Aeson
import GHC.Generics

import qualified Go.Board.Default as D
import Go.Game
import Go.Player
import Go.Run.JSON

data ServerMessageRep = ServerMessageD_9_2 (AssociatedServerMessage (D.Board 9 2))
                      | ServerMessageD_13_2 (AssociatedServerMessage (D.Board 13 2))
                      | ServerMessageRepFail String
  deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON ServerMessageRep
instance ToJSON ServerMessageRep

data ServerMessage = ServerMessageGameStateRep GameStateRep
                   | ServerMessagePlayerRep (Maybe PlayerRep)
                   | ServerMessageFail String
  deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON ServerMessage
instance ToJSON ServerMessage

--gs :: ServerMessageRep -> GameStateRep

data ClientMessageRep = ClientMessageD_9_2 (AssociatedClientMessage (D.Board 9 2))
                      | ClientMessageD_13_2 (AssociatedClientMessage (D.Board 13 2))
                      | ClientMessageRepFail String
  deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON ClientMessageRep
instance ToJSON ClientMessageRep

data ClientMessage = ClientMessageActionRep GameStateRep
                   | ClientMessagePlayerRep (Maybe PlayerRep)
                   | ClientMessageFail String
  deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON ClientMessage
instance ToJSON ClientMessage
