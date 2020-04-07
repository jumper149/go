{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main where

import Data.Char as C (toLower)
import Data.Functor.Identity (runIdentity)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Vector as V
import GHC.Generics
import GHC.TypeLits
import Miso as Miso
import Miso.String
import Miso.Svg as Svg
import Text.Read (readMaybe)

import qualified Go.Board.Default as D
import qualified Go.Config as G
import qualified Go.Game.Game as G
import qualified Go.Game.Playing as G
import qualified Go.Game.Player as G
import qualified Go.Game.State as G

main :: IO ()
main = startApp App {..}
  where
    initialAction = Id
    model  = G.def
    update = updateModel
    view   = viewModel
    events = defaultEvents
    subs   = []
    mountPoint = Nothing

data Model = Model { gamestate :: G.GameState (D.BoardSquare 2) D.Coord 2
                   , coord     :: Maybe D.Coord
                   }
  deriving (Eq, Ord, Generic, Read, Show)

instance G.Default Model where
  def = Model { gamestate = either undefined id $ runIdentity $ G.runConfiguredT G.def G.initState
              , coord = Nothing
              }

data Action = Id
            | UpdateCoord MisoString
            | SubmitPlace
            | SubmitPass
  deriving (Eq, Ord, Generic, Read, Show)

updateModel :: Action -> Model -> Effect Action Model
updateModel action m =
  case action of
    Id -> noEff m
    UpdateCoord mstr -> noEff $ m { coord = readMaybe . fromMisoString $ mstr }
    SubmitPlace -> case coord m of
                     Nothing -> undefined
                     Just c -> noEff $ m { gamestate = G.doTurn G.def (G.Place c) (gamestate m)
                                         , coord = Nothing
                                         }
    SubmitPass -> noEff $ m { gamestate = G.doTurn G.def G.Pass (gamestate m)
                            , coord = Nothing
                            }

viewModel :: Model -> View Action
viewModel x =
  div_ [
       ] [ viewBoard . G.currentBoard . gamestate $ x
         , div_ [ class_ "control"
                ] [ input_ [ type_ "text"
                           , autofocus_ True
                           , onInput UpdateCoord
                           ]
                  , button_ [ disabled_ (isNothing (coord x))
                            , onClick SubmitPlace
                            ] [ text "Place" ]
                  , button_ [ onClick SubmitPass
                            ] [ text "Pass" ]
                  ]
         ]

viewBoard :: D.BoardSquare 2 -> View Action
viewBoard (D.BSquare _ v) = svg_ [ class_ "board" , viewBox_ "0 0 19 19" ] $ (Prelude.concat . V.toList $ V.imap viewStone v)

viewStone :: KnownNat n => Int -> G.Stone (G.PlayerN n) -> [View Action]
viewStone _ G.Free = []
viewStone i (G.Stone p) = [ circle_ [ fill_ (ms $ playerColor p) , cx_ $ ms x , cy_ $ ms y , r_ "0.5" ] []]
  where y = toEnum (i `div` 19) + 0.5 :: Double
        x = toEnum (i `mod` 19) + 0.5 :: Double

data Color = Black
           | White
           | Red
           | Blue
           | Green
  deriving (Bounded, Enum, Eq, Ord, Generic, Read, Show)

playerColor :: KnownNat n => G.PlayerN n -> String
playerColor p = fmap C.toLower . show $ fromMaybe Red $ M.lookup p playerColorMap

playerColorMap :: forall n. KnownNat n => M.Map (G.PlayerN n) Color
playerColorMap = M.fromList $ Prelude.zip [minBound .. (maxBound :: G.PlayerN n)] [minBound .. (maxBound :: Color)]
