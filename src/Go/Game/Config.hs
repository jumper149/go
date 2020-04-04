module Go.Game.Config ( Default (..)
                      , Config (..)
                      , Malconfig (..)
                      , ConfiguredT
                      , runConfiguredT
                      ) where

import Control.Monad.Except
import Control.Monad.Reader

import Go.Game.Rules

-- | A class for configuration related types with default values.
class Default a where
  def :: a

instance Default Rules where
  def = Rules { passing = Allowed
              , ko = Ko Forbidden
              , suicide = Allowed
              }

type ConfiguredT m a = ExceptT Malconfig (ReaderT Config m) a

runConfiguredT :: Config -> ConfiguredT m a -> m (Either Malconfig a)
runConfiguredT config = flip runReaderT config . runExceptT

-- | The configuration of a game.
data Config = Config { size  :: Int
                     , rules :: Rules
                     }
  deriving (Eq, Show)

instance Default Config where
  def = Config { size = 19
               , rules = def
               }

data Malconfig = MalconfigSize
  deriving (Eq, Show)
