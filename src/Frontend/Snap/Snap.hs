{-# LANGUAGE OverloadedStrings #-}

module Frontend.Snap.Snap ( SnapGame (..)
                          , startSnapServer
                          ) where

import Rules
import GameState

import qualified Data.ByteString as BS
import Control.Applicative
import Snap.Core
import Snap.Http.Server

startSnapServer :: [(String, Snap ())] -> IO ()
startSnapServer routes = quickHttpServe $ frontPage routes

frontPage :: [(String, Snap ())] -> Snap ()
frontPage routes = ifTop (writeBS "go")
               <|> route (map (\ (x,y) -> (read x :: BS.ByteString , y)) routes)

class (Game b c p, Show b, Show p) => SnapGame b c p where

  startSnap :: Snap (b,p)
  startSnap = start stepSnap endSnap

  stepSnap :: GameState b p -> Snap (Action c)

  endSnap :: EndScreen b p -> Snap (b,p)
