{-# LANGUAGE KindSignatures #-}

module Go.Game.Player ( PlayerN
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

-- | Return the next player. Helper function for 'Go.Game.State.act'.
next :: forall n. KnownNat n => PlayerN n -> PlayerN n
next player = if player == (maxBound :: PlayerN n)
                 then minBound
                 else succ player
