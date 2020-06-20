module Main ( main
            ) where

import qualified Test.Board.Default

main :: IO ()
main = Test.Board.Default.runTests
