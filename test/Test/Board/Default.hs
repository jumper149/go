{-# LANGUAGE DataKinds #-}

module Test.Board.Default where

import Test.Hspec
import Test.QuickCheck

import Go.Board.Default
import Go.Game.Config
import Go.Game.Game
import Go.Game.Player

runTests :: IO ()
runTests = hspec $ do
  describe "Board.Default" $ do

    let emptyBoard = empty def :: Maybe (BoardSquare 2)
    it "creates an empty board" $
      property $ prop_empty <$> emptyBoard

    it "lets you place single stones" $
      property $ prop_single <$> emptyBoard

    it "updates the board properly" $
      property $ prop_remove <$> emptyBoard


newtype ArbCoordXY = ArbCoordXY CoordXY
  deriving (Bounded, Eq, Ord, Read, Show)

instance Arbitrary ArbCoordXY where
  arbitrary = elements $ maybe [] (fmap ArbCoordXY . coords) (empty def :: Maybe (BoardSquare 2))
  shrink = shrinkNothing


prop_empty :: BoardSquare 2 -> Bool
prop_empty board = all (== Free) $ getStone board <$> coords board

prop_single :: BoardSquare 2 -> ArbCoordXY -> ArbCoordXY -> Bool
prop_single board (ArbCoordXY coord1) (ArbCoordXY coord2) = coordsSame == stonesSame
  where stonesSame = getStone (putStone board coord1 (Stone black)) coord2 == Stone black
        coordsSame = coord1 == coord2

prop_remove :: BoardSquare 2 -> ArbCoordXY -> Bool
prop_remove board (ArbCoordXY coord) = getStone newBoard coord == Free
  where single = putStone board coord $ Stone white
        surrounded = foldl (\ b xy -> putStone b xy $ Stone black) single $ libertyCoords board coord
        newBoard = updateBoard surrounded black

white :: PlayerN 2
white = minBound

black :: PlayerN 2
black = next minBound
