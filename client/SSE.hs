module SSE where

import Miso

import qualified Go.Game.State as G

import Operation

handleSSE :: SSE (G.GameState b c n) -> Operation b c n
handleSSE (SSEMessage gs) = SetState gs
handleSSE SSEClose = undefined
handleSSE SSEError = undefined
