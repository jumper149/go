module Test.Board.Default where

import Test.Hspec
import Test.QuickCheck

import Go.Board.Default
import Go.Game.Game
import Go.Game.Player

runTests :: IO ()
runTests = hspec $ do
  describe "Board.Default" $ do

    let emptyBoard = empty :: BoardSquare 13 2
    it "creates an empty board" $
      property $ prop_empty emptyBoard

    it "lets you place single stones" $
      property $ prop_single emptyBoard

    it "updates the board properly" $
      property $ prop_remove emptyBoard


newtype ArbCoord = ArbCoord (Coord 13)
  deriving (Eq, Ord, Read, Show)

instance Arbitrary ArbCoord where
  arbitrary = elements $ fmap ArbCoord $ coords $ (empty :: BoardSquare 13 2)
  shrink = shrinkNothing


prop_empty :: BoardSquare 13 2 -> Bool
prop_empty board = all (== Free) $ getStone board <$> coords board

prop_single :: BoardSquare 13 2 -> ArbCoord -> ArbCoord -> Bool
prop_single board (ArbCoord coord1) (ArbCoord coord2) = coordsSame == stonesSame
  where stonesSame = getStone (putStone board coord1 (Stone black)) coord2 == Stone black
        coordsSame = coord1 == coord2

prop_remove :: BoardSquare 13 2 -> ArbCoord -> Bool
prop_remove board (ArbCoord coord) = getStone newBoard coord == Free
  where single = putStone board coord $ Stone white
        surrounded = foldl (\ b xy -> putStone b xy $ Stone black) single $ libertyCoords board coord
        newBoard = updateBoard surrounded black

white :: PlayerN 2
white = minBound

black :: PlayerN 2
black = next minBound

-- TODO: remove when implemented in QuickCheck >= 2.13
instance Testable prop => Testable (Maybe prop) where
  property = property . liftMaybe
    where
      liftMaybe Nothing = property Discard
      liftMaybe (Just prop) = property prop
