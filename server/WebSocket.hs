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
import ServerState.Class
import WebSocket.Message

import Go.Message

websocketMiddleware :: MonadBaseControlIdentity IO m
                    => MiddlewareT (ServerStateT m)
websocketMiddleware = websocketsOrT defaultConnectionOptions serverApp

-- TODO: maybe ping every 30 seconds to keep alive?
serverApp :: MonadBaseControl IO m
          => ServerAppT (ServerStateT m)
serverApp pendingConnection = liftedBracket connect disconnect hold
    where connect = do conn <- liftBase $ acceptRequest pendingConnection
                       transact $ addClient conn
          hold clientId = do initLobby clientId
                             loopLobby clientId
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
                ClientMessageActionRep action -> (uncurry serverSendMessage =<<) . transact $ do
                                                   rs <- recipients <$> readPlayer <*> readPlayers
                                                   gs <- actGame action
                                                   let answer = ServerMessageGameStateRep . gameState <$> gs
                                                   return (rs , answer)
                ClientMessagePlayerRep mbP -> (uncurry serverSendMessage =<<) . transact $ do
                                                rs <- recipients <$> readPlayer <*> pure mempty
                                                p <- updatePlayer mbP -- TODO: p should be equal to mbP, keep it like this?
                                                let answer = ServerMessagePlayerRep <$> p
                                                return (rs , answer)
                _ -> liftBase $ putStrLn "not a game message, but already in game" -- TODO: server side error log would be better
              loopGame

initLobby :: MonadBase IO m
          => ClientId
          -> ServerStateT m ()
initLobby c = (uncurry serverSendMessage =<<) . transact $ do
                rs <- recipients <$> getClient c <*> pure mempty
                gss <- gameSetList
                let msg = Right $ ServerMessageRepLobby gss
                return (rs , msg)

loopLobby :: MonadBase IO m
          => ClientId
          -> ServerStateT m ()
loopLobby k = do c <- transact $ getClient k
                 msg <- serverReceiveMessage c
                 liftBase . C8.putStrLn $ encode msg -- TODO: remove?
                 case msg of
                   ClientMessageRepCreateGame config -> (uncurry serverSendMessage =<<) . transact $ do
                                                       gss <- addGameSet config
                                                       let msg = ServerMessageRepLobby <$> gss
                                                           rs = recipients c mempty -- TODO: client connection isnt refreshed but taken from beginning of this loop
                                                       return (rs , msg)
                   ClientMessageRepTryConfig config -> serverSendMessage (recipients c mempty) $ ServerMessageRepApproveConfig <$> tryConfig config
                   ClientMessageRepPromote gameId -> runGameSetT k gameId $ do
                                                       initGame
                                                       loopGame
                   _ -> liftBase $ putStrLn "not a lobby message, but in lobby" -- TODO: server side error log would be better
                 loopLobby k

liftedBracket :: MonadBaseControl IO m
              => m a
              -> (a -> m b)
              -> (a -> m c)
              -> m c
liftedBracket before after body = control $ \ runInBase ->
                                    bracket (runInBase before)
                                    (\ saved -> runInBase (after =<< restoreM saved))
                                    (\ saved -> runInBase (body =<< restoreM saved))
