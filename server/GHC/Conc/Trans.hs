module GHC.Conc.Trans ( transact
                      ) where

import Control.Monad.Base
import Control.Monad.Trans.Control.Functor
import GHC.Conc

import Clients.Internal

transact :: (MonadBase IO m, MonadTransFunctor t)
         => t STM a
         -> t m a
transact = liftMap $ liftBase . atomically
