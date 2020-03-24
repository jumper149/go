{-# LANGUAGE DataKinds, TypeOperators #-}

module Go.Client.JSONTerm ( clientJSONTerm
                          ) where

import Data.Proxy (Proxy)
import Servant
import Servant.Client
import Network.HTTP.Client (newManager, defaultManagerSettings)

import Go.Frontend.Term
import Go.Game.End
import Go.Game.Rules
import Go.Game.State
import Go.Server.JSON

-- TODO move rules?
clientJSONTerm :: forall b c p. (JSONGame b c p, TermGame b c p) => IO (EndScreen b p)
clientJSONTerm = do manager' <- newManager defaultManagerSettings
                    let clientEnv = mkClientEnv manager' $ BaseUrl Http "localhost" port ""
                    let turn = do mbGs <- runClientM renderQ clientEnv
                                  case mbGs of
                                    Left err -> error $ "ClientError occured: " <> show err
                                    Right gs -> do render gs
                                                   mbAct <- action gs
                                                   case mbAct of
                                                     Left ExceptRedo -> turn
                                                     Left ExceptEnd -> undefined -- TODO ???
                                                     Right act -> runClientM (playQ act) clientEnv >> turn
                    turn
  where renderQ :<|> playQ = client (api :: Proxy (API b c p))
        port = 8501 -- TODO move away from here
