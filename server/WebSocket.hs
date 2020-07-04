{-# LANGUAGE FlexibleContexts #-}

module WebSocket ( websocketMiddleware
                 ) where

import Control.Exception (bracket)
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Monad.Trans.Control.Identity
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as C8 (putStrLn)
import Data.Foldable (traverse_)
import GHC.Conc
import Network.WebSockets
import Network.Wai.Handler.WebSockets.Trans
import Network.Wai.Trans (MiddlewareT)

import Clients.Class
import GameSet
import GameSet.Class
import Message
import ServerState
import ServerState.Message

import Go.Message

websocketMiddleware :: MonadBaseControlIdentity IO m
                    => GameId
                    -> MiddlewareT (ServerStateT m)
websocketMiddleware gameId = websocketsOrT defaultConnectionOptions $ serverApp gameId

-- TODO: maybe ping every 30 seconds to keep alive?
serverApp :: MonadBaseControl IO m
          => GameId
          -> ServerAppT (ServerStateT m)
serverApp gameId pendingConnection = liftedBracket connect disconnect hold
    where connect = do conn <- liftBase $ acceptRequest pendingConnection
                       transact $ addClient conn
          hold clientId = runGameSetT clientId gameId $ do initGame
                                                           loopGame
          disconnect clientId = transact $ removeClient clientId

initGame :: MonadBase IO m
         => GameSetT m ()
initGame = (uncurry serverSendMessage =<<) . transact $ do
             rs <- recipients <$> readPlayer <*> pure mempty
             gs <- readGameSet
             let msg = Right . ServerMessageGameStateRep $ gameState gs
             return (rs , msg)

-- | A loop, that receives messages via the websocket and also answers back, while progressing the
-- 'GameState'.
loopGame :: MonadBase IO m
         => GameSetT m ()
loopGame = do msg <- serverReceiveMessage =<< transact readPlayer
              liftBase . C8.putStrLn $ encode msg -- TODO: remove?
              case msg of
                ClientMessageRepFail _ -> liftBase $ putStrLn "failed to do action" -- TODO: server side error log would be better
                ClientMessageActionRep action -> let transaction = do rs <- recipients <$> readPlayer <*> readPlayers
                                                                      gs <- actGame action
                                                                      let answer = ServerMessageGameStateRep . gameState <$> gs
                                                                      return (rs , answer)
                                                 in uncurry serverSendMessage =<< transact transaction
                ClientMessagePlayerRep mbP -> undefined
              loopGame

liftedBracket :: MonadBaseControl IO m
              => m a
              -> (a -> m b)
              -> (a -> m c)
              -> m c
liftedBracket before after body = control $ \ runInBase ->
                                    bracket (runInBase before)
                                    (\ saved -> runInBase (after =<< restoreM saved))
                                    (\ saved -> runInBase (body =<< restoreM saved))
