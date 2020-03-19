import Test.QuickCheck

import Board.Default
import Game

prop_reverse :: [Int] -> Bool
prop_reverse xs = (reverse xs) == xs

main :: IO ()
main = quickCheck prop_reverse
