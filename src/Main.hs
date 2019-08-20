{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Main where

import GameState
import qualified DefaultBoard as D

import Control.Monad ( void
                     )

main :: IO ()
main = void (startTerm :: IO (GameState D.BoardSquare D.PlayerBW))
