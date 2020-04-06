module Go.Config ( Default (..)
                 , Config (..)
                 , Malconfig (..)
                 , ConfiguredT
                 , runConfiguredT
                 ) where

import Control.Monad.Except
import Control.Monad.Reader
import GHC.Generics

import Go.Game.Rules

-- | A class for configuration related types with default values.
class Default a where
  -- | The default value.
  def :: a

instance Default Rules where
  def = Rules { passing = Allowed
              , ko = Ko Forbidden
              , suicide = Allowed
              }

type ConfiguredT m a = ExceptT Malconfig (ReaderT Config m) a

-- | Run a configured computation by supplying a 'Config'.
runConfiguredT :: Config -> ConfiguredT m a -> m (Either Malconfig a)
runConfiguredT config = flip runReaderT config . runExceptT

-- | The configuration of a game.
data Config = Config { players :: Int
                     , rules :: Rules
                     , size  :: Int
                     }
  deriving (Eq, Generic, Ord, Read, Show)

instance Default Config where
  def = Config { players = 2
               , rules = def
               , size = 19
               }

-- | Exceptions that can thrown if configuration with 'Config' doesn't work.
data Malconfig = MalconfigSize
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)
