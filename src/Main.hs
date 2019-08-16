{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Main where

import qualified Board as B
import qualified DefaultBoard as D

import Control.Monad ( void
                     )

main :: IO ()
main = void (B.startGame :: IO (D.Board,D.Player))
