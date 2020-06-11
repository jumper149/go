{-# LANGUAGE TypeOperators #-}

module API where

import Servant
import Servant.HTML.Lucid (HTML)
import Servant.RawM (RawM)

import Html

type API =             Get '[HTML] GameHtml
      :<|> "wss"    :> RawM
      :<|> "public" :> Raw

api :: Proxy API
api = Proxy
