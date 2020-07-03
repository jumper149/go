module Go.Player ( PlayerRep (..)
                 , matchingPlayerRep
                 ) where

import GHC.Generics
import Data.Aeson (FromJSON, ToJSON)

import qualified Go.Board.Default as D
import Go.Game.Game

data PlayerRep = PlayerD_9_2 (AssociatedPlayer (D.Board 9 2))
               | PlayerD_13_2 (AssociatedPlayer (D.Board 13 2))
  deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON PlayerRep
instance ToJSON PlayerRep

matchingPlayerRep :: PlayerRep -> PlayerRep -> Bool
matchingPlayerRep p1 p2 = case (p1,p2) of
                            (PlayerD_9_2 _,PlayerD_9_2 _) -> True
                            (PlayerD_13_2 _,PlayerD_13_2 _) -> True
                            _ -> False
