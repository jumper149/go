{-# LANGUAGE DataKinds, TypeOperators #-}

module Go.Server.JSON ( JSONGame
                      , serverJSON
                      , API
                      , api
                      ) where

import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Proxy (Proxy)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import Servant


import Go.Game.End
import Go.Game.Game
import Go.Game.Playing
import Go.Game.Rules
import Go.Game.State

type API b c p = "create"                                        :> Post '[JSON] FilePath
            :<|> "render" :> ReqBody '[JSON] FilePath            :> Post '[JSON] (GameState b c p)
            :<|> "play"   :> ReqBody '[JSON] (FilePath,Action c) :> Post '[JSON] ()

api :: Proxy (API b c p)
api = Proxy

class (Game b c p, Generic b, Generic c, Generic p, FromJSON b, FromJSON c, FromJSON p, ToJSON b, ToJSON c, ToJSON p) => JSONGame b c p

server :: forall b c p. JSONGame b c p => Rules -> Server (API b c p)
server rules = createH :<|> renderH :<|> playH
  where createH = do liftIO . B.writeFile path . encode $ (initState :: GameState b c p)
                     return path
          where path = "GameServ.json"
        renderH path = do gs <- liftIO $ decode <$> B.readFile path
                          maybe undefined return gs
        playH (path,action) = do gs <- liftIO $ decode <$> B.readFile path
                                 let newGs = doTurn rules action <$> (gs :: Maybe (GameState b c p))
                                 maybe undefined (liftIO . B.writeFile path . encode) newGs

serverJSON :: forall b c p. JSONGame b c p => Rules -> IO (EndScreen b p)
serverJSON rules = do putStrLn $ "Port is " <> show port
                      run port app
                      return undefined
  where port = 8501
        app = serve api (server rules :: Server (API b c p))
