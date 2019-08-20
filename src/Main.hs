{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Main where

import Frontend.Term.Term
import qualified Board.Default as D

import Control.Monad ( void
                     )

main :: IO ()
main = void (startTerm :: IO (D.BoardSquare , D.PlayerBW))
