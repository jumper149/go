module Test.Board.Default where

import Test.Hspec
import Test.QuickCheck

import Board.Default
import Game

runTests :: IO ()
runTests = hspec $ do
  describe "Board.Default" $ do

    let emptyBoard = empty :: BoardSquare
    it "creates an empty board" $
      property $ prop_empty emptyBoard

    it "lets you place single stones" $
      property $ prop_single emptyBoard

    it "updates the board properly" $
      property $ prop_remove emptyBoard


newtype ArbCoordXY = ArbCoordXY CoordXY
  deriving Show

instance Arbitrary ArbCoordXY where
  arbitrary = elements $ ArbCoordXY <$> coords (empty :: BoardSquare)
  shrink = shrinkNothing


prop_empty :: BoardSquare -> Bool
prop_empty board = all (== Free) $ getStone board <$> coords board

prop_single :: BoardSquare -> ArbCoordXY -> ArbCoordXY -> Bool
prop_single board (ArbCoordXY coord1) (ArbCoordXY coord2) = coordsSame == stonesSame
  where stonesSame = getStone (putStone board coord1 (Stone Black)) coord2 == Stone Black
        coordsSame = coord1 == coord2

prop_remove :: BoardSquare -> ArbCoordXY -> Bool
prop_remove board (ArbCoordXY coord) = getStone newBoard coord == Free
  where single = putStone board coord $ Stone White
        surrounded = foldl (\ b xy -> putStone b xy $ Stone Black) single $ libertyCoords board coord
        newBoard = updateBoard surrounded Black
