module Operation ( Operation (..)
                 , handleSSE
                 , handleWS
                 ) where

import Miso

import GHC.Generics

import qualified Go.Game.State as G

data Operation b c n = NoOp
                     | QueueOp [Operation b c n]
                     | UpdateAction (Maybe (G.Action c))
                     | SubmitAction
                     | SetState (G.GameState b c n)
  deriving (Eq, Ord, Generic, Read, Show)

handleSSE :: SSE (G.GameState b c n) -> Operation b c n
handleSSE (SSEMessage gs) = SetState gs
handleSSE SSEClose = NoOp
handleSSE SSEError = NoOp

handleWS :: WebSocket m -> Operation b c n
handleWS _ = NoOp
