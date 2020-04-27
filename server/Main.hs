{-# LANGUAGE FlexibleContexts, TypeOperators #-}

module Main where

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson
import Data.Default.Class
import Data.Proxy (Proxy)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (Port, run)
import Servant
import System.Console.GetOpt
import System.Environment (getArgs)

import qualified Go.Board.Default as D
import Go.Game.Config
import Go.Game.End
import Go.Game.Game
import Go.Game.Playing
import Go.Game.State
import Go.Run.JSON

newtype OptArgs = OptArgs { port :: Port
                          }
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

instance Default OptArgs where
  def = OptArgs { port = 8022
                }

options :: [OptDescr (OptArgs -> IO OptArgs)]
options = [ Option ['p'] ["port"]
              (ReqArg
                (\ arg opt -> return opt { port = read arg })
                "port")
              "Specify port to listen on"
          ]

-- TODO: use?: https://cabal.readthedocs.io/en/latest/cabal-package.html?highlight=getDataDir#accessing-data-files-from-package-code
main :: IO ()
main = do args <- getArgs
          let (optArgs , nonOptArgs , _) = getOpt Permute options args
          opts <- foldl (>>=) (return def) optArgs
          let OptArgs { port = port
                      } = opts
              path = head nonOptArgs
          putStrLn $ "Path: " <> path
          run port $ serve (Proxy :: Proxy Raw) $ serveDirectoryWebApp path

--main :: IO ()
--main = void (serverJSON def :: IO (EndScreen (D.BoardSquare 2) 2))

type API b c n = "render"                               :> Get '[JSON] (GameState b c n)
            :<|> "play"   :> ReqBody '[JSON] (Action c) :> Post '[JSON] ()

api :: Proxy (API b c n)
api = Proxy

type AppM b c n = ReaderT (MVar (GameState b c n)) Handler

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
