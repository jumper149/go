{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Main where

import Frontend.Term.Term
import qualified Board.Default as D
import qualified Board.Loop as L

import Control.Monad ( void
                     )

main :: IO ()
main = void (startTerm :: IO (D.BoardSquare , D.PlayerBW))
