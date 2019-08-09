{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Main where

import qualified Board as B
import qualified DefaultBoard as D

main :: IO ()
main = (B.startGame :: IO (D.Board,D.Player)) >> return ()
