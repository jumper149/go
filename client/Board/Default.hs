module Board.Default ( viewBoard
                     ) where

import qualified Data.Vector as V
import GHC.TypeLits
import qualified Miso as Miso
import qualified Miso.String as MisoS
import Miso.Svg as Svg

import qualified Go.Board.Default as D
import qualified Go.Game.Game as G
import qualified Go.Game.Player as G

import Player

viewBoard :: D.BoardSquare 2 -> Miso.View action
viewBoard (D.BSquare s v) = svg_ [ Miso.class_ "board"
                                 , viewBox_ . MisoS.unwords . fmap MisoS.ms $ [ minDim1 , minDim1 , lenDim1 , lenDim1 ]
                                 ] $ viewGrid boardLenDim1
                                  <> viewCoords
                                  <> (Prelude.concat . V.toList $ V.imap (viewStone boardLenDim1) v)
  where lenDim1 = toEnum boardLenDim1 + margin - minDim1 :: Double
        minDim1 = 1 - margin
        margin = 1.5 -- for coordinate markers with 'viewCoords'
        boardLenDim1 = fromEnum s

viewGrid :: Int -> [Miso.View action]
viewGrid n = case coordsDim1 of
               [] -> []
               cs -> fmap (\ y -> gridLine (head cs) (last cs) y y) cs
                  <> fmap (\ x -> gridLine x x (head cs) (last cs)) cs
  where gridLine x1 x2 y1 y2 = line_ [ stroke_ "black" , strokeWidth_ "0.1" ,  x1_ x1 , x2_ x2 , y1_ y1 , y2_ y2 ] []
        coordsDim1 = fmap MisoS.ms . take n $ [ (1 :: Int) .. ]

-- TODO: Make svg chars and import them here. Function will be similar to 'viewGrid'.
viewCoords :: [Miso.View action]
viewCoords = []

viewStone :: KnownNat n
          => Int                   -- ^ boardsize
          -> Int                   -- ^ index of stone in boardvector
          -> G.Stone (G.PlayerN n)
          -> [Miso.View action]
viewStone size i s = case s of
                  G.Free -> [ rect_ [ x_ . MisoS.ms $ x - 0.5
                                    , y_ . MisoS.ms $ y - 0.5
                                    , width_ "1"
                                    , height_ "1"
                                    , fill_ "green"
                                    , fillOpacity_ "0.5"
                                    ] []
                            ]
                  G.Stone p -> [ circle_ [ fill_ (MisoS.ms $ colorize p)
                                         , cx_ $ MisoS.ms x
                                         , cy_ $ MisoS.ms y
                                         , r_ "0.5"
                                         ] []
                               ]
  where y = toEnum $ (i `div` size) + 1 :: Double
        x = toEnum $ (i `mod` size) + 1 :: Double
