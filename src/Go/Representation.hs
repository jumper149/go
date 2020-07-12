{-# LANGUAGE AllowAmbiguousTypes #-}

module Go.Representation ( RepresentableGame (..)

                         , BoardRep (..)

                         , GameStateRep (..)
                         , ActionRep (..)
                         , actRep
                         , initStateRep
                         , getCurrentPlayerRep

                         , PlayerRep (..)
                         , matchingPlayerRep
                         ) where

import qualified Go.Board.Default as D

import Go.Game.Game
import Go.Game.State
import Go.Representation.Board
import Go.Representation.Game
import Go.Representation.Player

class Game b => RepresentableGame b where
  fromBoardRep :: BoardRep -> Maybe b
  toBoardRep :: b -> BoardRep

  fromGameStateRep :: GameStateRep -> Maybe (AssociatedGameState b)
  toGameStateRep :: AssociatedGameState b -> GameStateRep

  fromActionRep :: ActionRep -> Maybe (AssociatedAction b)
  toActionRep :: AssociatedAction b -> ActionRep

  fromPlayerRep :: PlayerRep -> Maybe (AssociatedPlayer b)
  toPlayerRep :: AssociatedPlayer b -> PlayerRep

instance RepresentableGame (D.Board 9 2) where
  fromBoardRep r = case r of
                     BoardD_9_2 b -> Just b
                     _ -> Nothing
  toBoardRep = BoardD_9_2
  fromGameStateRep r = case r of
                     GameStateD_9_2 b -> Just b
                     _ -> Nothing
  toGameStateRep = GameStateD_9_2
  fromActionRep r = case r of
                     ActionD_9_2 b -> Just b
                     _ -> Nothing
  toActionRep = ActionD_9_2
  fromPlayerRep r = case r of
                     PlayerD_9_2 b -> Just b
                     _ -> Nothing
  toPlayerRep = PlayerD_9_2

instance RepresentableGame (D.Board 13 2) where
  fromBoardRep r = case r of
                     BoardD_13_2 b -> Just b
                     _ -> Nothing
  toBoardRep = BoardD_13_2
  fromGameStateRep r = case r of
                     GameStateD_13_2 b -> Just b
                     _ -> Nothing
  toGameStateRep = GameStateD_13_2
  fromActionRep r = case r of
                     ActionD_13_2 b -> Just b
                     _ -> Nothing
  toActionRep = ActionD_13_2
  fromPlayerRep r = case r of
                     PlayerD_13_2 b -> Just b
                     _ -> Nothing
  toPlayerRep = PlayerD_13_2
