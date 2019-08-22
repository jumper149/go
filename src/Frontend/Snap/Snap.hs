{-# LANGUAGE OverloadedStrings #-}

module Frontend.Snap.Snap ( SnapGame (..)
                          , startSnapServer
                          ) where

import Rules
import GameState

import qualified Data.ByteString as BS
import Data.String (fromString)
import Control.Applicative
import Snap.Core
import Snap.Http.Server

startSnapServer :: [(String, Snap ())] -> IO ()
startSnapServer routes = httpServe config $ site routes
  where config = setPort 8000 mempty

site :: [(String, Snap ())] -> Snap ()
site routes = ifTop (writeBS "go")
          <|> route (map (\ (x,y) -> (fromString x , y)) routes)

class (Game b c p, Show b, Show p) => SnapGame b c p where

  startSnap :: Snap (b,p)
  startSnap = start stepSnap endSnap

  stepSnap :: GameState b p -> Snap (Action c)
  stepSnap state = do writeBS . fromString . show $ currBoard state
                      return Pass

  endSnap :: EndScreen b p -> Snap (b,p)
  endSnap endScr = return (lastBoard endScr , winner endScr)
