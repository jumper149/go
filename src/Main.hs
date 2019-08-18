{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Main where

import GameState
import qualified DefaultBoard as D

import Control.Monad ( void
                     )

main :: IO ()
main = void (startTerm :: IO (D.BoardSquare,D.PlayerBW))
