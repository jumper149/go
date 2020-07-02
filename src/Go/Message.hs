module Go.Message ( ServerMessageRep (..)
                  , ClientMessageRep (..)
                  ) where

import Data.Aeson
import GHC.Generics

import qualified Go.Board.Default as D
import Go.Run.JSON

data ServerMessageRep = ServerMessageD_9_2 (ServerMessage (D.Board 9 2))
                      | ServerMessageD_13_2 (ServerMessage (D.Board 13 2))
                      | ServerMessageRepFail String
  deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON ServerMessageRep where
instance ToJSON ServerMessageRep where

data ClientMessageRep = ClientMessageD_9_2 (ClientMessage (D.Board 9 2))
                      | ClientMessageD_13_2 (ClientMessage (D.Board 13 2))
                      | ClientMessageRepFail String
  deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON ClientMessageRep where
instance ToJSON ClientMessageRep where
