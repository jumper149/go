module Operation ( Operation (..)
                 , LobbyOp (..)
                 ) where

import GHC.Generics

import Game.Operation

data Operation b c n = NoOp
                     | QueueOp [Operation b c n]
                     | GameOp (GameOperation b c n)
                     | LobbyOp LobbyOp
                     | WriteErrorLog String
  deriving (Eq, Ord, Generic, Read, Show)

data LobbyOp = AskAvailableGames
             | JoinGame String
  deriving (Eq, Ord, Generic, Read, Show)
