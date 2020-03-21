{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Rules ( RulesetEnvT (..)
             , runRulesetEnvT
             , Rules (..)
             , Permission (..)
             , KoRule (..)
             , defaultRules
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

defaultRules = Rules { passing = Allowed
                     , ko = Ko Forbidden
                     , suicide = Allowed
                     }

newtype RulesetEnvT m a = RulesetEnvT (ReaderT Rules m a)
    deriving (Functor, Applicative, Monad, MonadReader Rules)

instance MonadTrans RulesetEnvT where
    lift = RulesetEnvT . lift

runRulesetEnvT :: Rules -> RulesetEnvT m a -> m a
runRulesetEnvT rules (RulesetEnvT r) = runReaderT r rules
