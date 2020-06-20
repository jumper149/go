{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Html ( GameHtml (..)
            ) where

import GHC.Generics
import qualified Lucid as L
import qualified Lucid.Base as L
import qualified Data.Text as T

newtype GameHtml = GameHtml { jsAppPath :: T.Text }
  deriving (Eq, Generic, Ord, Read, Show)

instance L.ToHtml GameHtml where
  toHtmlRaw = L.toHtml
  toHtml GameHtml {..} = L.doctypehtml_ $ do
                           L.meta_ [L.charset_ "utf-8"]
                           L.body_ $ jsRef jsAppPath
    where jsRef href = L.with (L.script_ mempty)
                         [ L.makeAttribute "src" href
                         , L.makeAttribute "async" mempty
                         , L.makeAttribute "defer" mempty
                         ]
