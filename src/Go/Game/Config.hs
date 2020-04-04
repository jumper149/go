{-# LANGUAGE FunctionalDependencies #-}

module Go.Game.Config ( Default (..)
                      , Config (..)
                      ) where

import Go.Game.Rules

-- | The configuration of a game.
data Config = Config { size  :: Int
                     , rules :: Rules
                     }
  deriving Eq

instance Default Config where
  def = Config { size = 19
               , rules = def
               }

-- | A class for configuration related types with default values.
class Default a where
  def :: a

instance Default Rules where
  def = Rules { passing = Allowed
              , ko = Ko Forbidden
              , suicide = Allowed
              }
