{-# LANGUAGE RecordWildCards #-}

module GameSet.Internal ( GameSet (..)
                        , GameId
                        , Players
                        , actGameSet
                        , addPlayerTo
                        , removePlayerFrom
                        , playerListFrom
                        , BadConfigServer (..)
                        , embedBadConfig
                        , errorGameSetNotFound -- TODO: remove when not needed anymore
                        ) where

import qualified Data.Map as M
import GHC.Generics
import Servant (FromHttpApiData, ToHttpApiData) -- TODO: maybe write these instances manually?

import Clients.Class (ClientId)

import Go.Config
import Go.Game
import Go.GameId
import Go.Player

newtype Players = Players { unwrapPlayers :: M.Map ClientId (Maybe PlayerRep) }
  deriving (Eq, Generic, Monoid, Ord, Read, Semigroup, Show)

data GameSet = GameSet { gameConfig :: Config
                       , gameIdentification :: GameId
                       , gamePlayers :: Players
                       , gameState :: GameStateRep
                       }
  deriving (Eq, Generic, Ord, Read, Show)

actGameSet :: ClientId
           -> ActionRep
           -> GameSet
           -> Either BadConfigServer GameSet
actGameSet k a gs = if isCurrentPlayer k gs
                          then fmap (updateGameState gs) . embedBadConfig . configure (gameConfig gs) . actRep a $ gameState gs
                          else Left BadConfigMismatch
  where updateGameState set state = set { gameState = state }

-- | Check if given 'ClientId' is the 'currentPlayer'. Helper function for 'updateGameSet'.
isCurrentPlayer :: ClientId
                -> GameSet
                -> Bool
isCurrentPlayer key gs = Just (Just currentPlayer) == clientPlayer
  where currentPlayer = getCurrentPlayerRep $ gameState gs
        clientPlayer = M.lookup key . unwrapPlayers $ gamePlayers gs

-- | Add a 'Maybe PlayerRep' to a 'GameSet', but only if it fits the 'AssociatedPlayer'.
addPlayerTo :: ClientId
            -> Maybe PlayerRep
            -> GameSet
            -> Either BadConfigServer GameSet
addPlayerTo c p gs = if correctPlayerRep -- TODO: very maybe this sanity checking could be done by the typechecker instead
                        then Right $ gs { gamePlayers = Players . M.insert c p . unwrapPlayers $ gamePlayers gs }
                        else Left BadConfigMismatch
  where correctPlayerRep = case p of
                             Nothing -> True
                             Just p -> matchingPlayerRep p . getCurrentPlayerRep $ gameState gs

-- | Remove a player from a 'GameSet'. Acts like 'id' when no player is found.
removePlayerFrom :: ClientId -> GameSet -> GameSet
removePlayerFrom c gs = gs { gamePlayers = Players . M.delete c . unwrapPlayers $ gamePlayers gs } -- TODO: keep no sanity checks?

playerListFrom :: GameSet -> [(ClientId, Maybe PlayerRep)]
playerListFrom = M.toList . unwrapPlayers . gamePlayers

data BadConfigServer = BadConfigInternal BadConfig
                     | BadConfigMismatch
  deriving (Eq, Generic, Ord, Read, Show)

embedBadConfig :: Either BadConfig a -> Either BadConfigServer a
embedBadConfig (Left bc) = Left $ BadConfigInternal bc
embedBadConfig (Right a) = Right a

errorGameSetNotFound :: GameId -> a
errorGameSetNotFound k = error $ "Can't find game with id: " <> show k
