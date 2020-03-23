{-# LANGUAGE DataKinds, TypeOperators #-}

module Client.JSONTerm ( clientJSONTerm
                       ) where

import Data.Aeson
import Data.Proxy (Proxy)
import GHC.Generics (Generic)
import Lucid
import Servant
import Servant.Client
import Network.HTTP.Client (newManager, defaultManagerSettings)

import Data.Either (fromRight)

import End
import Frontend.Term
import Game
import Rules
import Server.JSON
import State

type API b c p = "create"                                        :> Post '[JSON] FilePath
            :<|> "render" :> ReqBody '[JSON] FilePath            :> Post '[JSON] (GameState b c p)
            :<|> "play"   :> ReqBody '[JSON] (FilePath,Action c) :> Post '[JSON] ()

clientJSONTerm :: forall b c p. (JSONGame b c p, TermGame b c p) => Rules -> IO (EndScreen b p)
clientJSONTerm rules = do manager' <- newManager defaultManagerSettings
                          let clientEnv = mkClientEnv manager' $ BaseUrl Http "localhost" port ""
                          path <- fromRight undefined <$> runClientM createQ clientEnv
                          let turn = do gs <- fromRight undefined <$> runClientM (renderQ path) clientEnv
                                        render' (currentBoard gs) $ currentPlayer gs
                                        act <- action' $ currentBoard gs
                                        _ <- runClientM (playQ (path,act)) clientEnv
                                        turn
                          turn
                          return undefined
  where createQ :<|> renderQ :<|> playQ = client (api :: Proxy (API b c p))
        port = 8501