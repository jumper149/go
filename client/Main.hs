{-# LANGUAGE RecordWildCards #-}

module Main ( main
            ) where

import Data.Default.Class
import Data.Proxy
import Miso

import qualified Game.Board.Default as D
import Game.Run
import Message
import Model
import Operation

main :: IO ()
main = run (Proxy :: Proxy (D.Board 19 2))

run :: forall b c n. MisoGame b c n => Proxy b -> IO ()
run _ = startApp App {..}
  where
    initialAction = NoOp
    model  = def :: Model b c n
    update = updateModel
    view   = viewModel
    events = defaultEvents
    subs   = let url = URL "ws://local.felixspringer.xyz:8022/wss"
                 protocols = Protocols []
                 handler = handleWS
             in [ websocketSub url protocols handler ]
    mountPoint = Nothing
