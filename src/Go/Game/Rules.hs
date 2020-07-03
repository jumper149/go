{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}

module Go.Game.Rules ( MonadRules (..)
                     , RulesT
                     , runRulesT
                     , Rules (..)
                     , Permission (..)
                     , KoRule (..)
                     , RuleViolation (..)
                     ) where

import Control.Monad.Except -- TODO: use Control.Monad.Throw?
import Control.Monad.Reader
import Data.Aeson (FromJSON, ToJSON)
import Data.Default.Class
import GHC.Generics

data Permission = Allowed
                | Forbidden
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

instance FromJSON Permission
instance ToJSON Permission

-- TODO change?
data KoRule = Ko Permission
            | SuperKo
  deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON KoRule
instance ToJSON KoRule

-- TODO improve
data Komi = Integral Int
          | PlusHalf Int
  deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON Komi
instance ToJSON Komi

data Rules = Rules { passing :: Permission
                   , ko      :: KoRule
                   , suicide :: Permission
                   }
  deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON Rules
instance ToJSON Rules

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

runRulesetEnvT :: Rules -> RulesetEnvT m a -> m a
runRulesetEnvT r rulesetEnv = runReaderT (unwrapRulesetEnvT rulesetEnv) r

-- | A data type representing exceptions, that may occur when checking rules
data RuleViolation = ExceptionEnd
                   | RuleViolationPassing
                   | RuleViolationNotFree
                   | RuleViolationSuicide
                   | RuleViolationKo
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

instance FromJSON RuleViolation
instance ToJSON RuleViolation

newtype RulesT m a = RulesT { unwrapRulesT :: ExceptT RuleViolation (RulesetEnvT m) a }
  deriving (Functor, Applicative, Monad, MonadError RuleViolation, MonadReader r)

-- | Evaluate a computation with exposure to the given 'Rules' and
-- handling exceptions with 'RuleViolation'.
runRulesT :: Rules -> RulesT m a -> m (Either RuleViolation a)
runRulesT r = runRulesetEnvT r . runExceptT . unwrapRulesT

instance MonadTrans RulesT where
  lift = RulesT . lift . lift

class MonadError RuleViolation m => MonadRules m where
  rules :: m Rules

instance Monad m => MonadRules (RulesT m) where
  rules = RulesT . lift . RulesetEnvT $ ask

instance MonadRules m => MonadRules (ReaderT e m) where -- TODO: more instances?
  rules = lift rules
