module Test.Board.Default where

import Test.Hspec
import Test.QuickCheck

import Go.Board.Default
import Go.Game.Game
import Go.Game.Player

runTests :: IO ()
runTests = hspec $
  describe "Board.Default" $ do

    let emptyBoard = empty :: Board 13 2
    it "create an empty board" $
      property $ prop_empty emptyBoard

    it "place single stones" $
      property $ prop_single emptyBoard

    it "update the board" $
      property $ prop_remove emptyBoard


newtype ArbCoord = ArbCoord (Coord 13)
  deriving (Eq, Ord, Read, Show)

instance Arbitrary ArbCoord where
  arbitrary = elements $ ArbCoord <$> coords
  shrink = shrinkNothing


prop_empty :: Board 13 2 -> Bool
prop_empty board = all (== Free) $ getStone board <$> coords

prop_single :: Board 13 2 -> ArbCoord -> ArbCoord -> Bool
prop_single board (ArbCoord coord1) (ArbCoord coord2) = coordsSame == stonesSame
  where stonesSame = getStone (putStone board coord1 (Stone black)) coord2 == Stone black
        coordsSame = coord1 == coord2

prop_remove :: Board 13 2 -> ArbCoord -> Bool
prop_remove board (ArbCoord coord) = getStone newBoard coord == Free
  where single = putStone board coord $ Stone white
        surrounded = foldl (\ b xy -> putStone b xy $ Stone black) single $ libertyCoords coord
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
