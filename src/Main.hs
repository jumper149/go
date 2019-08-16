{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Main where

import qualified Game as G
import qualified DefaultBoard as D

import Control.Monad ( void
                     )

main :: IO ()
main = void (G.startGame :: IO (D.Board,D.Player))
