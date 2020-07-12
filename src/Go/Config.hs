module Go.Config ( MonadConfig (..)
                 , Config (..)
                 , BoardName (..)
                 , PlayerCount (..)
                 , BoardSize (..)
                 , BadConfig (..)
                 , ConfigT
                 , runConfigT
                 , configure
                 , embedRuleViolation
                 ) where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.OrphanInstances ()
import Data.Default.Class
import Data.Finite
import GHC.Generics

import Go.Game.Act
import Go.Game.Rules

newtype PlayerCount = PlayerCount (Finite 16)
  deriving (Bounded, Enum, Eq, FromJSON, Generic, Num, Ord, Read, Show, ToJSON)

newtype BoardSize = BoardSize (Finite 19)
  deriving (Bounded, Enum, Eq, FromJSON, Generic, Num, Ord, Read, Show, ToJSON)

data BoardName = Default
               | Loop
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

instance FromJSON BoardName
instance ToJSON BoardName

-- | The configuration of a game.
data Config = Config { board :: BoardName
                     , players :: PlayerCount
                     , ruleset :: Rules
                     , size  :: BoardSize
                     }
  deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON Config
instance ToJSON Config

instance Default Config where
  def = Config { board = Default
               , players = 2
               , ruleset = def
               , size = 9
               }

newtype ConfigT m a = ConfigT { unwrapConfigT :: ExceptT BadConfig (ReaderT Config m) a }
  deriving (Applicative, Functor, Monad, MonadError BadConfig) -- TODO: more instances

instance MonadTrans ConfigT where
  lift = ConfigT . lift . lift

-- | Run a configured computation by supplying a 'Config'.
runConfigT :: Config -> ConfigT m a -> m (Either BadConfig a)
runConfigT c = flip runReaderT c . runExceptT . unwrapConfigT

configure :: Config -> ConfigT Identity a -> Either BadConfig a
configure c = runIdentity . runConfigT c

-- | Exceptions that can be thrown if configuration with 'Config' doesn't work.
data BadConfig = BadConfigActionMismatch
               | BadConfigBoard
               | BadConfigPlayers
               | BadConfigRuleset RuleViolation
               | BadConfigSize
  deriving (Eq, Generic, Ord, Read, Show)

class MonadError BadConfig m => MonadConfig m where
  config :: m Config

instance Monad m => MonadConfig (ConfigT m) where
  config = ConfigT $ lift ask

embedRuleViolation :: MonadConfig m => Either RuleViolation a -> m a
embedRuleViolation (Left r) = throwError $ BadConfigRuleset r
embedRuleViolation (Right a) = return a
