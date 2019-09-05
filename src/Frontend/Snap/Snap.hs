{-# LANGUAGE OverloadedStrings #-}

module Frontend.Snap.Snap ( SnapGame (..)
                          , startSnapServer
                          ) where

import Class
import GameState

import Data.String (fromString)
import qualified Data.ByteString as BS
import Control.Applicative
import Snap.Core
import Snap.Http.Server

startSnapServer :: [(String, Snap ())] -> IO ()
startSnapServer routes = httpServe config $ site routes
  where config = setPort 8000 mempty

site :: [(String, Snap ())] -> Snap ()
site routes = ifTop (writeBS "go")
          <|> route (map (\ (x,y) -> (fromString x , y)) routes)

placeHandler :: Snap ()
placeHandler = do param <- getParam "coord"
                  maybe (writeBS "lmao")
                        writeBS param

class (Game b c p, Show b, Show p) => SnapGame b c p where

  startSnap :: Snap (b,p)
  startSnap = do state <- stepSnap startManually :: Snap (GameState b p)
                 return (currBoard state , currPlayer state)

  stepSnap :: GameState b p -> Snap (GameState b p)
  stepSnap state = do writeBS . fromString . show $ currBoard state
                      route [ ("place/:placeparam" , placeHandler) ]
                      return state
    where useRules :: Snap ()
          useRules = do param <- getParam "placeparam"
                        maybe (writeBS "bs")
                              redirect param
                        return ()

  endSnap :: EndScreen b p -> Snap (b,p)
  endSnap endScr = return (lastBoard endScr , winner endScr)
