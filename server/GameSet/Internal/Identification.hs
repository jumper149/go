module GameSet.Internal.Identification ( GameId
                                       ) where

import GHC.Generics

-- extra module to fix cyclic dependency is Client.Internal and GameSet.Internal

newtype GameId = GameId { unwrapGameId :: Integer }
  deriving (Enum, Eq, Generic, Ord, Read, Show)
