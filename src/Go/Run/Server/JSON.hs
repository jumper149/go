{-# LANGUAGE FlexibleContexts, TypeOperators #-}

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

import Go.Config
import Go.Game.End
import Go.Game.Game
import Go.Game.Playing
import Go.Game.State

class (Game b c n, Generic b, Generic c, FromJSON b, FromJSON c, ToJSON b, ToJSON c) => JSONGame b c n

type API b c n = "render"                               :> Get '[JSON] (GameState b c n)
            :<|> "play"   :> ReqBody '[JSON] (Action c) :> Post '[JSON] ()

type AppM b c n = ReaderT (MVar (GameState b c n)) Handler

api :: Proxy (API b c n)
api = Proxy

server :: forall b c n. JSONGame b c n => Config -> ServerT (API b c n) (AppM b c n)
server config = renderH :<|> playH
  where renderH :: AppM b c n (GameState b c n)
        renderH = liftIO . readMVar =<< ask

        playH :: Action c -> AppM b c n ()
        playH action = do gs <- ask
                          liftIO $ modifyMVar_ gs f
          where f = return . doTurn (rules config) action

serverJSON :: forall b c n. JSONGame b c n => Config -> IO (EndScreen b n)
serverJSON config = do putStrLn $ "Port is " <> show port
                       initial <- either (error . show) id <$> runConfiguredT config initState :: IO (GameState b c n)
                       gs <- newMVar initial
                       let app = serve api $ hoistServer api (\ r -> runReaderT r gs) (server config)
                       run port app
                       return undefined -- TODO: undefined behaviour
  where port = 8501
