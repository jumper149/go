module Test.Board.Default where

import Test.QuickCheck

import Board.Default
import Game

runTests :: IO ()
runTests = quickCheck prop_empty

prop_empty :: Bool
prop_empty = all (== Free) $ getStone board <$> coords board
  where board = empty :: BoardSquare
