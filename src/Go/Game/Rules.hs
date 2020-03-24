module Go.Game.Rules ( RulesetEnvT
                     , runRulesetEnvT
                     , Rules (..)
                     , defaultRules
                     , Permission (..)
                     , KoRule (..)
                     ) where

import Control.Monad.Reader

data Permission = Allowed
                | Forbidden
                deriving (Eq, Show)

-- TODO change?
data KoRule = Ko Permission
            | SuperKo
            deriving (Eq, Show)

-- TODO improve
data Komi = Integral Int
          | PlusHalf Int
          deriving (Eq, Show)

data Rules = Rules { passing :: Permission
                   , ko :: KoRule
                   , suicide :: Permission
                   }

defaultRules :: Rules
defaultRules = Rules { passing = Allowed
                     , ko = Ko Forbidden
                     , suicide = Allowed
                     }

newtype RulesetEnvT m a = RulesetEnvT { unwrapRulesetEnvT :: ReaderT Rules m a }
    deriving (Functor, Applicative, Monad, MonadReader Rules)

instance MonadTrans RulesetEnvT where
    lift = RulesetEnvT . lift

runRulesetEnvT :: Rules -> RulesetEnvT m a -> m a
runRulesetEnvT rules rulesetEnv = runReaderT (unwrapRulesetEnvT rulesetEnv) rules
