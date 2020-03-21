{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Rules ( RulesetEnvT (..)
             , runRulesetEnvT
             , Rules (..)
             , Permission (..)
             , KoRule (..)
             ) where

import Control.Monad.Reader

data Permission = Allowed
                | Forbidden
                deriving (Eq, Show)

data KoRule = Ko Permission
            | SuperKo
            deriving (Eq, Show)

data Komi = Integral Int
          | PlusHalf Int
          deriving (Eq, Show)

data Rules = Rules { passing :: Permission
                   , ko :: KoRule
                   , suicide :: Permission
                   }

newtype RulesetEnvT m a = RulesetEnvT { unwrapRulesetEnvT :: ReaderT Rules m a }
    deriving (Functor, Applicative, Monad, MonadReader Rules)

instance MonadTrans RulesetEnvT where
    lift = RulesetEnvT . lift

runRulesetEnvT :: Rules -> RulesetEnvT m a -> m a
runRulesetEnvT rules rulesetEnv = runReaderT (unwrapRulesetEnvT rulesetEnv) rules
