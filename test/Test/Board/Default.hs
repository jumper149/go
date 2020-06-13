module Test.Board.Default where

import Test.Hspec
import Test.QuickCheck

import GHC.TypeLits

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


newtype ArbCoord i = ArbCoord (Coord i)
  deriving (Eq, Ord, Read, Show)

instance KnownNat i => Arbitrary (ArbCoord i) where
  arbitrary = elements $ ArbCoord <$> coords
  shrink = shrinkNothing


prop_empty :: (KnownNat i, KnownNat n) => Board i n -> Bool
prop_empty board = all (== Free) $ getStone board <$> coords

prop_single :: (KnownNat i, KnownNat n) => Board i n -> ArbCoord i -> ArbCoord i -> Bool
prop_single board (ArbCoord coord1) (ArbCoord coord2) = coordsSame == stonesSame
  where stonesSame = getStone (putStone board coord1 (Stone black)) coord2 == Stone black
        coordsSame = coord1 == coord2

prop_remove :: (KnownNat i, KnownNat n) => Board i n -> ArbCoord i -> Bool
prop_remove board (ArbCoord coord) = getStone newBoard coord == Free
  where single = putStone board coord $ Stone white
        surrounded = foldl (\ b xy -> putStone b xy $ Stone black) single $ libertyCoords coord
        newBoard = updateBoard surrounded black

white :: KnownNat i => PlayerN i
white = minBound

black :: KnownNat i => PlayerN i
black = next minBound

-- TODO: remove when implemented in QuickCheck >= 2.13
instance Testable prop => Testable (Maybe prop) where
  property = property . liftMaybe
    where
      liftMaybe Nothing = property Discard
      liftMaybe (Just prop) = property prop
