{-# LANGUAGE KindSignatures #-}

module Go.Game.Player ( PlayerN
                      , countPlayers
                      , next
                      ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Proxy
import GHC.Generics
import GHC.TypeLits

-- | A parametrized data type holding n players.
data PlayerN (n :: Nat) = PlayerN { playerNo :: Integer }
  deriving (Eq, Ord, Generic, Read, Show)

instance FromJSON (PlayerN n)
instance ToJSON (PlayerN n)

instance KnownNat n => Bounded (PlayerN n) where
  minBound = PlayerN 1
  maxBound = let i = natVal (Proxy :: Proxy n)
              in if i >= playerNo (minBound :: PlayerN n)
                    then PlayerN i
                    else undefined

instance KnownNat n => Enum (PlayerN n) where
  fromEnum (PlayerN i) = fromEnum i
  toEnum i = let p = PlayerN $ toEnum i
              in if p >= (minBound :: PlayerN n) && p <= (maxBound :: PlayerN n)
                    then p
                    else undefined

-- | Count the number of players. Helper function for 'Go.Game.State.checkPassing'.
countPlayers :: forall n. KnownNat n => PlayerN n -> Int                            -- TODO: don't give player as argument
countPlayers _ = fromEnum (maxBound :: PlayerN n)

-- | Return the next player. Helper function for 'Go.Game.State.act'.
next :: forall n. KnownNat n => PlayerN n -> PlayerN n
next player = if player == (maxBound :: PlayerN n)
                 then minBound
                 else succ player
