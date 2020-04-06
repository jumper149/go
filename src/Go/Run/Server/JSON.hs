{-# LANGUAGE FlexibleContexts, TypeOperators #-}

module Go.Run.Server.JSON ( JSONGame
                          , API
                          , api
                          ) where

import Data.Aeson
import Data.Proxy
import GHC.Generics
import Servant

import Go.Game.Game
import Go.Game.State

class (Game b c n, Generic b, Generic c, FromJSON b, FromJSON c, ToJSON b, ToJSON c) => JSONGame b c n

type API b c n = "render"                               :> Get '[JSON] (GameState b c n)
            :<|> "play"   :> ReqBody '[JSON] (Action c) :> Post '[JSON] ()

api :: Proxy (API b c n)
api = Proxy
