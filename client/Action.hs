module Action where

import GHC.Generics
import Miso.String

data Action = NoOp
            | UpdateCoord MisoString
            | SubmitPlace
            | SubmitPass
  deriving (Eq, Ord, Generic, Read, Show)
