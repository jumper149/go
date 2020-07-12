module WebSocket.Message.Internal ( WSServerMessage (..)
                                  , WSClientMessage (..)
                                  ) where

import Data.Aeson
import Data.Maybe (fromMaybe)
import GHC.Generics
import Network.WebSockets

import Go.Run.Message

newtype WSServerMessage = WSServerMessage { unwrapWSServerMessage :: ServerMessage }
  deriving (Eq, Generic, Ord, Read, Show)

-- TODO: instance only covers text, not binary!
instance WebSocketsData WSServerMessage where
  fromDataMessage (Text bs _) = fromLazyByteString bs
  fromLazyByteString = WSServerMessage . fromMaybe (ServerMessageFail mempty) . decode
  toLazyByteString = encode . unwrapWSServerMessage

newtype WSClientMessage = WSClientMessage { unwrapWSClientMessage :: ClientMessage }
  deriving (Eq, Generic, Ord, Read, Show)

-- TODO: instance only covers text, not binary!
instance WebSocketsData WSClientMessage where
  fromDataMessage (Text bs _) = fromLazyByteString bs
  fromLazyByteString = WSClientMessage . fromMaybe (ClientMessageFail mempty) . decode
  toLazyByteString = encode . unwrapWSClientMessage
