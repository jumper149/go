module Go.Board ( BoardRep (..)
                , ActionRep (..)
                , GameStateRep (..)
                , actRep
                ) where

import GHC.Generics

import qualified Go.Board.Default as D
import Go.Game.Act
import Go.Game.Rules
import Go.Game.State

data BoardRep = BoardD_9_2 (D.Board 9 2)
              | BoardD_13_2 (D.Board 13 2)
  deriving Generic

data ActionRep = ActionD_9_2 (AssociatedAction (D.Board 9 2))
               | ActionD_13_2 (AssociatedAction (D.Board 13 2))
  deriving Generic

data GameStateRep = GameStateD_9_2 (AssociatedGameState (D.Board 9 2))
                  | GameStateD_13_2 (AssociatedGameState (D.Board 13 2))
  deriving Generic

actRep :: Rules -> ActionRep -> GameStateRep -> Maybe (Either RuleViolation GameStateRep)
actRep rules action gamestate =
    case (action , gamestate) of
      (ActionD_9_2 a , GameStateD_9_2 gs) -> Just $ GameStateD_9_2 <$> act rules a gs
      (ActionD_13_2 a , GameStateD_13_2 gs) -> Just $ GameStateD_13_2 <$> act rules a gs
      _ -> Nothing
