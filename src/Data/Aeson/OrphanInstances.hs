{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Aeson.OrphanInstances where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Maybe (fromJust)
import Data.Finite
import qualified Data.Vector.Sized as V
import GHC.TypeLits

instance KnownNat n => FromJSON (Finite n) where
  parseJSON = fmap finite . parseJSON
instance KnownNat n => ToJSON (Finite n) where
  toJSON = toJSON . getFinite

instance (KnownNat n, FromJSON a) => FromJSON (V.Vector n a) where
  parseJSON = fmap (fromJust . V.toSized) . parseJSON
instance (KnownNat n, ToJSON a) => ToJSON (V.Vector n a) where
  toJSON = toJSON . V.fromSized
