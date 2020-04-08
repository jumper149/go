{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main where

import Data.Functor.Identity (runIdentity)
import Data.Maybe (isNothing)
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

import Player

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
viewBoard (D.BSquare _ v) = svg_ [ class_ "board" , viewBox_ "-0.5 -0.5 21 21" ] $ viewGrid <> viewCoords <> (Prelude.concat . V.toList $ V.imap viewStone v)

-- TODO: Add thicc lines.
viewGrid :: [View Action]
viewGrid = fmap (\ x -> line_ [ stroke_ "black" , strokeWidth_ "0.1" , x1_ (ms x) , x2_ (ms x) , y1_ "1" , y2_ "19" ] []) dim1Coords<>
           fmap (\ y -> line_ [ stroke_ "black" , strokeWidth_ "0.1" , x1_ "1" , x2_ "19" , y1_ (ms y) , y2_ (ms y) ] []) dim1Coords
  where dim1Coords = [ 1 .. (19 :: Int) ]

-- TODO: Make svg chars and import them here.
viewCoords :: [View Action]
viewCoords = fmap (\ y -> ellipse_ [  cx_ "0" , cy_ (ms y) , rx_ "0.4" , ry_ "0.3" ] []) dim1Coords
          <> fmap (\ y -> ellipse_ [  cx_ "20" , cy_ (ms y) , rx_ "0.4" , ry_ "0.3" ] []) dim1Coords
          <> fmap (\ x -> ellipse_ [  cx_ (ms x) , cy_ "0" , rx_ "0.3" , ry_ "0.4" ] []) dim1Coords
          <> fmap (\ x -> ellipse_ [  cx_ (ms x) , cy_ "20" , rx_ "0.3" , ry_ "0.4" ] []) dim1Coords
  where dim1Coords = [ 1 .. (19 :: Int) ]

viewStone :: KnownNat n => Int -> G.Stone (G.PlayerN n) -> [View Action]
viewStone _ G.Free = []
viewStone i (G.Stone p) = [ circle_ [ fill_ (ms $ colorize p) , cx_ $ ms x , cy_ $ ms y , r_ "0.5" ] []]
  where y = (i `div` 19) + 1 :: Int
        x = (i `mod` 19) + 1 :: Int
