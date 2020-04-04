{-# LANGUAGE DataKinds, TypeOperators #-}

module Go.Run.Server.JSON ( JSONGame
                          , serverJSON
                          , API
                          , api
                          ) where

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson
import Data.Proxy (Proxy)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import Servant

import Go.Game.Config
import Go.Game.End
import Go.Game.Game
import Go.Game.Playing
import Go.Game.State

class (Game b c p, Generic b, Generic c, Generic p, FromJSON b, FromJSON c, FromJSON p, ToJSON b, ToJSON c, ToJSON p) => JSONGame b c p

type API b c p = "render"                               :> Get '[JSON] (GameState b c p)
            :<|> "play"   :> ReqBody '[JSON] (Action c) :> Post '[JSON] ()

type AppM b c p = ReaderT (MVar (GameState b c p)) Handler

api :: Proxy (API b c p)
api = Proxy

server :: forall b c p. JSONGame b c p => Config -> ServerT (API b c p) (AppM b c p)
server config = renderH :<|> playH
  where renderH :: AppM b c p (GameState b c p)
        renderH = liftIO . readMVar =<< ask

        playH :: Action c -> AppM b c p ()
        playH action = do gs <- ask
                          liftIO $ modifyMVar_ gs f
          where f = return . doTurn (rules config) action

serverJSON :: forall b c p. JSONGame b c p => Config -> IO (EndScreen b p)
serverJSON config = do putStrLn $ "Port is " <> show port
                       gs <- newMVar (maybe undefined id (initState config) :: GameState b c p) -- TODO: undefined behaviour
                       let app = serve api $ hoistServer api (\ r -> runReaderT r gs) (server config)
                       run port app
                       return undefined -- TODO: undefined behaviour
  where port = 8501
