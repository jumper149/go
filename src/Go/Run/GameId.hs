module Go.Run.GameId ( GameId
                     ) where

import GHC.Generics

import Data.Aeson
import Servant.API (FromHttpApiData, ToHttpApiData)

newtype GameId = GameId { unwrapGameId :: Integer }
  deriving (Enum, Eq, Generic, Ord, Read, Show, FromHttpApiData, ToHttpApiData)

instance FromJSON GameId
instance ToJSON GameId
