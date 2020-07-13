{-# LANGUAGE RecordWildCards #-}

module Go.Run.Html ( GameHtml (..)
                   ) where

import GHC.Generics
import qualified Lucid as L
import qualified Lucid.Base as L
import qualified Data.Text as T

data GameHtml = GameHtml { cssPath :: T.Text
                         , jsAppPath :: T.Text
                         }
  deriving (Eq, Generic, Ord, Read, Show)

instance L.ToHtml GameHtml where
  toHtmlRaw = L.toHtml
  toHtml GameHtml {..} = L.doctypehtml_ $ do
                           L.meta_ [ L.charset_ "utf-8" ]
                           cssRef
                           L.body_ $ jsRef
    where jsRef = L.with (L.script_ mempty)
                    [ L.makeAttribute "src" jsAppPath
                    , L.makeAttribute "async" mempty
                    , L.makeAttribute "defer" mempty
                    ]
          cssRef = L.link_ [ L.rel_ "stylesheet"
                           , L.type_ "text/css"
                           , L.href_ cssPath
                           ]
