{-# LANGUAGE UndecidableInstances #-}

module Go.Game.Rules ( MonadRules (..)
                     , RulesetEnvT
                     , runRulesetEnvT
                     , Rules (..)
                     , Permission (..)
                     , KoRule (..)
                     ) where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Default.Class
import GHC.Generics

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
  deriving (Functor, Applicative, Monad, MonadError e, MonadTrans) -- TODO: more deriving?

instance MonadReader r m => MonadReader r (RulesetEnvT m) where
  ask = lift ask
  local f = RulesetEnvT . mapReaderT (local f) . unwrapRulesetEnvT

-- | Evaluate a computation with exposure to the given 'Rules'.
runRulesetEnvT :: Rules -> RulesetEnvT m a -> m a
runRulesetEnvT r rulesetEnv = runReaderT (unwrapRulesetEnvT rulesetEnv) r

class Monad m => MonadRules m where
  rules :: m Rules

instance Monad m => MonadRules (RulesetEnvT m) where
  rules = RulesetEnvT $ ask

instance MonadRules m => MonadRules (ExceptT e m) where
  rules = lift rules

instance MonadRules m => MonadRules (ReaderT e m) where -- TODO: more instances?
  rules = lift rules
