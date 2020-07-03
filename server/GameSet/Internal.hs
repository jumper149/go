{-# LANGUAGE FlexibleContexts #-}

module GameSet.Internal ( GameSet (..) -- TODO: limit access
                        , BadConfigServer (..)
                        , addPlayerTo
                        , removePlayerFrom
                        , updateGameSet
                        ) where

import GHC.Generics
import qualified Data.Map as M

import Clients.Class (ClientId)

import Go.Config
import Go.Game
import Go.Player

data BadConfigServer = BadConfigInternal BadConfig
                     | BadConfigMismatch
  deriving (Eq, Generic, Ord, Read, Show)

embedBadConfig :: Either BadConfig a -> Either BadConfigServer a
embedBadConfig (Left bc) = Left $ BadConfigInternal bc
embedBadConfig (Right a) = Right a

newtype Players = Players { unwrapPlayers :: M.Map ClientId PlayerRep }
  deriving (Eq, Generic, Monoid, Ord, Read, Semigroup, Show)

getPlayerFrom :: ClientId -> GameSet -> Maybe PlayerRep
getPlayerFrom k = M.lookup k . unwrapPlayers . gamePlayers

-- | Add a 'PlayerRep' to a 'GameSet', but only if it fits the 'AssociatedPlayer'.
addPlayerTo :: ClientId -> PlayerRep -> GameSet -> Either BadConfigServer GameSet
addPlayerTo c p gs = if correctPlayerRep -- TODO: very maybe this sanity checking could be done by the typechecker instead
                        then Right $ gs { gamePlayers = Players . M.insert c p . unwrapPlayers $ gamePlayers gs }
                        else Left BadConfigMismatch
    where correctPlayerRep = matchingPlayerRep p . getCurrentPlayerRep $ gameState gs

-- | Remove a player from a 'GameSet'. Acts like 'id' when no player is found.
removePlayerFrom :: ClientId -> GameSet -> GameSet
removePlayerFrom c gs = gs { gamePlayers = Players . M.delete c . unwrapPlayers $ gamePlayers gs } -- TODO: keep no sanity checks?

newtype GameId = GameId { unwrapGameId :: Integer }
  deriving (Enum, Eq, Generic, Ord, Read, Show)

data GameSet = GameSet { gameConfig :: Config
                       , gameId :: GameId
                       , gamePlayers :: Players
                       , gameState :: GameStateRep
                       }
  deriving (Eq, Generic, Ord, Read, Show)

updateGameSet :: ClientId
              -> ActionRep
              -> GameSet
              -> Either BadConfigServer GameSet
updateGameSet k a gs = if isCurrentPlayer k gs
                          then fmap (updateGameState gs) . embedBadConfig . configure (gameConfig gs) . actRep a $ gameState gs
                          else Left BadConfigMismatch
  where updateGameState set state = set { gameState = state }

-- | Check if given 'ClientId' is the 'currentPlayer'. Helper function for 'updateGameSet'.
isCurrentPlayer :: ClientId
                -> GameSet
                -> Bool
isCurrentPlayer key gs = any (currentPlayer ==) clientPlayer
  where currentPlayer = getCurrentPlayerRep $ gameState gs
        clientPlayer = getPlayerFrom key gs
