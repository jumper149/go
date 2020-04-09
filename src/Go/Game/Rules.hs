module Go.Game.Rules ( RulesetEnvT
                     , runRulesetEnvT
                     , Rules (..)
                     , Permission (..)
                     , KoRule (..)
                     ) where

import Data.Default.Class
import GHC.Generics

import Control.Monad.Reader

data Permission = Allowed
                | Forbidden
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

-- TODO change?
data KoRule = Ko Permission
            | SuperKo
  deriving (Eq, Generic, Ord, Read, Show)

-- TODO improve
data Komi = Integral Int
          | PlusHalf Int
  deriving (Eq, Generic, Ord, Read, Show)

data Rules = Rules { passing :: Permission
                   , ko      :: KoRule
                   , suicide :: Permission
                   }
  deriving (Eq, Generic, Ord, Read, Show)

instance Default Rules where
  def = Rules { passing = Allowed
              , ko = Ko Forbidden
              , suicide = Allowed
              }

newtype RulesetEnvT m a = RulesetEnvT { unwrapRulesetEnvT :: ReaderT Rules m a }
  deriving (Functor, Applicative, Monad, MonadReader Rules)

instance MonadTrans RulesetEnvT where
  lift = RulesetEnvT . lift

-- | Evaluate a computation with exposure to the given 'Rules'.
runRulesetEnvT :: Rules -> RulesetEnvT m a -> m a
runRulesetEnvT rules rulesetEnv = runReaderT (unwrapRulesetEnvT rulesetEnv) rules
