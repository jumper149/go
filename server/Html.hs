{-# LANGUAGE OverloadedStrings #-}

module Html where

import GHC.Generics
import qualified Lucid as L
import qualified Lucid.Base as L

data GameHtml = GameHtml
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

instance L.ToHtml GameHtml where
  toHtmlRaw = L.toHtml
  toHtml GameHtml = L.doctypehtml_ $ do
                      L.meta_ [L.charset_ "utf-8"]
                      L.body_ (jsRef "public/all.js")
    where jsRef href = L.with (L.script_ mempty)
                         [ L.makeAttribute "src" href
                         , L.makeAttribute "async" mempty
                         , L.makeAttribute "defer" mempty
                         ]
