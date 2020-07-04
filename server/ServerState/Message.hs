{-# LANGUAGE FlexibleContexts #-}

module ServerState.Message ( serverReceiveMessage
                           , serverSendMessage
                           , serverBroadcastMessage
                           ) where

import Control.Monad.Base
import GHC.Conc

import Clients.Class
import Message
import ServerState.Class
import ServerState

import Go.Message

-- | Read a message from the websocket.
serverReceiveMessage :: (MonadBase IO m, MonadServerState m)
                     => ClientId
                     -> m ClientMessageRep
serverReceiveMessage key = do conn <- connection <$> transact (getClient key)
                              unwrapWSClientMessage <$> liftBase (receiveData conn)

-- | Send a message via the websocket.
serverSendMessage :: MonadBase IO m
                  => ClientId
                  -> ServerStateT STM (Either String ServerMessageRep)
                  -> ServerStateT m ()
serverSendMessage key msgSTM = do (conn , msg) <- transact $ do
                                    conn <- connection <$> getClient key
                                    msg <- msgSTM
                                    return (conn , msg)
                                  case msg of
                                    Right rMsg -> liftBase $ sendTextData conn $ WSServerMessage rMsg
                                    Left lString -> liftBase $ sendTextData conn $ WSServerMessage $ ServerMessageFail lString

-- | Send a message to all clients in 'Clients' via the websocket.
serverBroadcastMessage :: MonadBase IO m
                       => [ClientId]
                       -> ServerStateT STM (Either String ServerMessageRep)
                       -> ServerStateT m ()
serverBroadcastMessage keys msgSTM = do (conns , msg , conn) <- transact $ do
                                          conns <- map (connection . snd) <$> clientList
                                          msg <- msgSTM
                                          conn <- connection <$> getClient key
                                          return (conns , msg , conn)
                                        case msg of
                                          Right rMsg -> traverse_ (\ c -> liftBase . sendTextData c $ WSServerMessage rMsg) conns
                                          Left lString -> liftBase . sendTextData conn $ WSServerMessage $ ServerMessageFail lString
