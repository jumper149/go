{-# LANGUAGE KindSignatures #-}

module Go.Game.Player ( PlayerN
                      , countPlayers
                      , next
                      ) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Finite
import GHC.Generics
import GHC.TypeLits

-- | A parametrized data type holding n players.
newtype PlayerN (n :: Nat) = PlayerN { playerNo :: Finite n }
  deriving (Bounded, Enum, Eq, Ord, Generic, Read, Show)

instance KnownNat n => FromJSON (PlayerN n) where
  parseJSON = fmap (PlayerN . finite) . parseJSON
instance KnownNat n => ToJSON (PlayerN n) where
  toJSON = toJSON . getFinite . playerNo

-- | Count the number of players. Helper function for 'Go.Game.State.checkPassing'.
countPlayers :: forall n. KnownNat n => PlayerN n -> Int                            -- TODO: don't give player as argument
countPlayers _ = fromEnum (maxBound :: PlayerN n) + 1 -- TODO: add +1 ??? currently otherwise passing once with 2 players ends the game

-- | Return the next player. Helper function for 'Go.Game.State.act'.
next :: forall n. KnownNat n => PlayerN n -> PlayerN n
next player = if player == (maxBound :: PlayerN n)
                 then minBound
                 else succ player
