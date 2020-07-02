module Message ( WSServerMessageRep (..)
               , WSClientMessageRep (..)
               ) where

import Data.Aeson
import Data.Maybe (fromMaybe)
import GHC.Generics
import Network.WebSockets

import Go.Message

newtype WSServerMessageRep = WSServerMessageRep { unwrapWSServerMessageRep :: ServerMessageRep }
  deriving (Eq, Generic, Ord, Read, Show)

-- TODO: instance only covers text, not binary!
instance WebSocketsData WSServerMessageRep where
  fromDataMessage (Text bs _) = fromLazyByteString bs
  fromLazyByteString = WSServerMessageRep . fromMaybe (ServerMessageRepFail mempty) . decode
  toLazyByteString = encode . unwrapWSServerMessageRep

newtype WSClientMessageRep = WSClientMessageRep { unwrapWSClientMessageRep :: ClientMessageRep }
  deriving (Eq, Generic, Ord, Read, Show)

-- TODO: instance only covers text, not binary!
instance WebSocketsData WSClientMessageRep where
  fromDataMessage (Text bs _) = fromLazyByteString bs
  fromLazyByteString = WSClientMessageRep . fromMaybe (ClientMessageRepFail mempty) . decode
  toLazyByteString = encode . unwrapWSClientMessageRep
