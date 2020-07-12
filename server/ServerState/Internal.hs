{-# LANGUAGE RecordWildCards #-}

module ServerState.Internal ( GameSets
                            , newGameSetFor
                            , addGameSetTo
                            , getGameSetFrom
                            , gameSetListFrom
                            ) where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import GHC.Generics

import Clients.Class (ClientId)
import GameSet.Class
import GameSet.Internal ( GameSet (..)
                        , embedBadConfig
                        , errorGameSetNotFound -- TODO: remove? and handle errors properly
                        )

import Go.Config
import Go.Representation

newtype GameSets = GameSets { unwrapGameSets :: M.Map GameId GameSet }
  deriving (Eq, Generic, Monoid, Ord, Read, Semigroup, Show)

newGameSetFor :: Config -> GameSets -> Either BadConfigServer GameSet
newGameSetFor gameConfig (GameSets gss) = do let gameIdentification = fromMaybe (toEnum 0) $ succ . fst <$> M.lookupMax gss
                                                 gamePlayers = mempty
                                             gameState <- embedBadConfig $ configure gameConfig initStateRep
                                             return GameSet {..}

addGameSetTo :: GameSet -> GameSets -> GameSets
addGameSetTo gs = GameSets . M.insert (gameIdentification gs) gs . unwrapGameSets

getGameSetFrom :: GameId -> GameSets -> Maybe GameSet
getGameSetFrom k = M.lookup k . unwrapGameSets

gameSetListFrom :: GameSets -> [(GameId,GameSet)]
gameSetListFrom = M.toList . unwrapGameSets
