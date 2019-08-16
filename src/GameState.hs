{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GameState ( GameState (..)
                 ) where

import qualified Game as G
import qualified DefaultBoard as D

data GameState = GState
               | Ended
