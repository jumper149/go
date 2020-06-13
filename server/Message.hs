module Message ( WSServerMessage (..)
               , WSClientMessage (..)
               ) where

import Data.Aeson
import Data.Maybe (fromMaybe)
import GHC.Generics
import Network.WebSockets

import Go.Run.JSON

newtype WSServerMessage b c n = WSServerMessage { unwrapWSServerMessage :: ServerMessage b c n }
  deriving (Eq, Generic, Ord, Read, Show)

-- TODO: instance only covers text, not binary!
instance JSONGame b c n => WebSocketsData (WSServerMessage b c n) where
  fromDataMessage (Text bs _) = fromLazyByteString bs
  fromLazyByteString = WSServerMessage . fromMaybe (ServerMessageFail mempty) . decode
  toLazyByteString = encode . unwrapWSServerMessage

newtype WSClientMessage b c n = WSClientMessage { unwrapWSClientMessage :: ClientMessage b c n }
  deriving (Eq, Generic, Ord, Read, Show)

-- TODO: instance only covers text, not binary!
instance JSONGame b c n => WebSocketsData (WSClientMessage b c n) where
  fromDataMessage (Text bs _) = fromLazyByteString bs
  fromLazyByteString = WSClientMessage . fromMaybe (ClientMessageFail mempty) . decode
  toLazyByteString = encode . unwrapWSClientMessage
