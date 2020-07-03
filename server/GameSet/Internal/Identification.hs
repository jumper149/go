module GameSet.Internal.Identification ( GameId
                                       ) where

import GHC.Generics

newtype GameId = GameId { unwrapGameId :: Integer }
  deriving (Enum, Eq, Generic, Ord, Read, Show)
