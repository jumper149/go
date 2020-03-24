{-# LANGUAGE DataKinds, TypeOperators #-}

module Go.Client.JSONTerm ( clientJSONTerm
                          ) where

import Data.Proxy (Proxy)
import Servant
import Servant.Client
import Network.HTTP.Client (newManager, defaultManagerSettings)

import Data.Either (fromRight)

import Go.Frontend.Term
import Go.Game.End
import Go.Game.Rules
import Go.Game.State
import Go.Server.JSON

-- TODO remove undefineds!!! Catch exceptions! What about rules???
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
  where createQ :<|> renderQ :<|> playQ = client (api :: Proxy (API b c p))
        port = 8501
