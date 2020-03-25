{-# LANGUAGE DataKinds, TypeOperators #-}

module Go.Run.Client.JSONTerm ( clientJSONTerm
                              ) where

import Data.Proxy (Proxy)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant
import Servant.Client

import Go.Game.End
import Go.Game.State
import Go.Run.Server.JSON
import Go.Run.Term

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
                                                     Right actn -> runClientM (playQ actn) clientEnv >> turn
                    turn
  where renderQ :<|> playQ = client (api :: Proxy (API b c p))
        port = 8501 -- TODO move away from here
