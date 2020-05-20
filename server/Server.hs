{-# LANGUAGE TypeOperators #-}

module Server where

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson
import Data.Default.Class
import Network.Wai.Handler.Warp (Port, run)
import Servant
import Servant.HTML.Lucid
import System.Directory (listDirectory)

import Html

import qualified Go.Board.Default as D
import Go.Game.Config
import Go.Game.End
import Go.Game.Game
import Go.Game.Playing
import Go.Game.State
import Go.Run.JSON

type API b c n =                                           Get '[HTML] GameHtml
            :<|> "render"                               :> Get '[JSON] (GameState b c n)
            :<|> "play"   :> ReqBody '[JSON] (Action c) :> Post '[JSON] ()
            :<|> "public"                               :> Raw

api :: Proxy (API b c n)
api = Proxy

type AppM b c n = ReaderT (MVar (GameState b c n)) Handler

handler :: forall b c n. JSONGame b c n => FilePath -> Config -> ServerT (API b c n) (AppM b c n)
handler path config = gameH :<|> renderH :<|> playH :<|> publicH
  where gameH :: Monad m => m GameHtml
        gameH = return GameHtml

        renderH :: AppM b c n (GameState b c n)
        renderH = liftIO . readMVar =<< ask

        playH :: Action c -> AppM b c n ()
        playH action = do gs <- ask
                          liftIO $ modifyMVar_ gs f
          where f = return . doTurn (rules config) action

        publicH :: ServerT Raw h
        publicH = serveDirectoryWebApp path

server :: forall b c n. JSONGame b c n => Port -> FilePath -> IO (EndScreen b n)
server port path = do putStrLn $ "Port is: " <> show port
                      putStrLn . ("Public files are: " <>) . unwords =<< listDirectory path

                      initial <- either (error . show) id <$> runConfiguredT config initState :: IO (GameState b c n) -- TODO: error?
                      gs <- newMVar initial

                      let app = serve api $ hoistServer api (\ r -> runReaderT r gs) (handler path config)
                      run port app
                      return undefined -- TODO: undefined behaviour
  where config = def
