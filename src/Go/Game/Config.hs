module Go.Game.Config ( Config (..)
                      , Malconfig (..)
                      , ConfiguredT
                      , runConfiguredT
                      , configure
                      ) where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Data.Default.Class
import GHC.Generics

import Go.Game.Rules

type ConfiguredT m a = ExceptT Malconfig (ReaderT Config m) a

-- | Run a configured computation by supplying a 'Config'.
runConfiguredT :: Config -> ConfiguredT m a -> m (Either Malconfig a)
runConfiguredT config = flip runReaderT config . runExceptT

configure :: Config -> ConfiguredT Identity a -> Either Malconfig a
configure config configured = runIdentity $ runConfiguredT config configured

-- | The configuration of a game.
data Config = Config { players :: Int
                     , ruleset :: Rules
                     , size  :: Int
                     }
  deriving (Eq, Generic, Ord, Read, Show)

instance Default Config where
  def = Config { players = 2
               , ruleset = def
               , size = 19
               }

-- | Exceptions that can thrown if configuration with 'Config' doesn't work.
data Malconfig = MalconfigSize
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)
