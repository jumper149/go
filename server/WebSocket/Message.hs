module WebSocket.Message ( serverReceiveMessage
                         , Recipients
                         , recipients
                         , serverSendMessage
                         ) where

import Control.Monad.Base
import Data.Foldable (traverse_)
import GHC.Conc
import GHC.Generics
import Network.WebSockets

import Clients.Class
import Message
import GameSet.Class
import ServerState.Class
import ServerState

import Go.Message

data Recipients = Recipients { mainRecipient :: Client
                             , allRecipients :: [Client] -- including the main recipient!
                             }
  deriving Generic

recipients :: Client -> [Client] -> Recipients
recipients c cs = if idC `elem` idCs
                     then Recipients c cs
                     else Recipients c $ c:cs
  where idC = identification c
        idCs = map identification cs

-- | Read a message from the websocket.
serverReceiveMessage :: MonadBase IO m
                     => Client
                     -> m ClientMessageRep
serverReceiveMessage c = unwrapWSClientMessageRep <$> liftBase (receiveData $ connection c)

-- | Send a message to all clients in 'Clients' via the websocket.
serverSendMessage :: MonadBase IO m
                  => Recipients
                  -> Either BadConfigServer ServerMessageRep
                  -> m ()
serverSendMessage rs msg = case msg of
                             Right rMsg -> traverse_ (\ c -> liftBase . sendTextData c $ WSServerMessageRep rMsg) conns
                             Left ex -> liftBase . sendTextData conn . WSServerMessageRep . ServerMessageRepFail $ show ex -- TODO: don't show, but have instance for message
  where conn = connection $ mainRecipient rs
        conns = connection <$> allRecipients rs
