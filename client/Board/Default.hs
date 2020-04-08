module Board.Default ( viewBoard
                     ) where

import qualified Data.Vector as V
import GHC.Generics
import GHC.TypeLits
import Miso as Miso
import Miso.String
import Miso.Svg as Svg

import qualified Go.Board.Default as D
import qualified Go.Game.Game as G
import qualified Go.Game.Player as G

import Player

viewBoard :: D.BoardSquare 2 -> View action
viewBoard (D.BSquare _ v) = svg_ [ class_ "board" , viewBox_ "-0.5 -0.5 21 21" ] $ viewGrid <> viewCoords <> (Prelude.concat . V.toList $ V.imap viewStone v)

-- TODO: Add thicc lines.
viewGrid :: [View action]
viewGrid = fmap (\ x -> line_ [ stroke_ "black" , strokeWidth_ "0.1" , x1_ (ms x) , x2_ (ms x) , y1_ "1" , y2_ "19" ] []) dim1Coords<>
           fmap (\ y -> line_ [ stroke_ "black" , strokeWidth_ "0.1" , x1_ "1" , x2_ "19" , y1_ (ms y) , y2_ (ms y) ] []) dim1Coords
  where dim1Coords = [ 1 .. (19 :: Int) ]

-- TODO: Make svg chars and import them here.
viewCoords :: [View action]
viewCoords = fmap (\ y -> ellipse_ [  cx_ "0" , cy_ (ms y) , rx_ "0.4" , ry_ "0.3" ] []) dim1Coords
          <> fmap (\ y -> ellipse_ [  cx_ "20" , cy_ (ms y) , rx_ "0.4" , ry_ "0.3" ] []) dim1Coords
          <> fmap (\ x -> ellipse_ [  cx_ (ms x) , cy_ "0" , rx_ "0.3" , ry_ "0.4" ] []) dim1Coords
          <> fmap (\ x -> ellipse_ [  cx_ (ms x) , cy_ "20" , rx_ "0.3" , ry_ "0.4" ] []) dim1Coords
  where dim1Coords = [ 1 .. (19 :: Int) ]

viewStone :: KnownNat n => Int -> G.Stone (G.PlayerN n) -> [View action]
viewStone _ G.Free = []
viewStone i (G.Stone p) = [ circle_ [ fill_ (ms $ colorize p) , cx_ $ ms x , cy_ $ ms y , r_ "0.5" ] []]
  where y = (i `div` 19) + 1 :: Int
        x = (i `mod` 19) + 1 :: Int
