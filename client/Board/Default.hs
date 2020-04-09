module Board.Default ( viewBoard
                     ) where

import qualified Data.Map as M
import qualified Data.Vector as V
import GHC.TypeLits
import qualified Miso as Miso
import qualified Miso.String as MisoS
import Miso.Svg as Svg

import qualified Go.Board.Default as D
import qualified Go.Game.Game as G
import qualified Go.Game.Player as G
import qualified Go.Game.State as G

import Operation
import Player

viewBoard :: KnownNat n => D.BoardSquare n -> Maybe D.Coord -> Miso.View (Operation D.Coord)
viewBoard (D.BSquare s v) c = svg_ [ Miso.style_ $ M.fromList [ ("background-color","grey")
                                                              , ("width","50%")
                                                              ]
                                   , viewBox_ . MisoS.unwords . fmap MisoS.ms $ [ minDim1 , minDim1 , lenDim1 , lenDim1 ]
                                   , onMouseOut $ UpdateAction Nothing
                                   ] $ viewGrid boardLenDim1
                                    <> viewCoordIndicator
                                    <> case c of
                                         Nothing -> []
                                         Just coord -> [ hintStone coord ]
                                    <> (Prelude.concat . V.toList $ V.imap (viewStone s) v)
  where lenDim1 = toEnum boardLenDim1 + margin - minDim1 :: Double
        minDim1 = 1 - margin
        margin = 1.5 -- for coordinate markers with 'viewCoordIndicator'
        boardLenDim1 = fromEnum s

viewGrid :: Int -> [Miso.View action]
viewGrid n = case coordsDim1 of
               [] -> []
               cs -> fmap (\ y -> gridLine (head cs) (last cs) y y) cs
                  <> fmap (\ x -> gridLine x x (head cs) (last cs)) cs
  where gridLine x1 x2 y1 y2 = line_ [ stroke_ "black" , strokeWidth_ "0.1" ,  x1_ x1 , x2_ x2 , y1_ y1 , y2_ y2 ] []
        coordsDim1 = fmap MisoS.ms . take n $ [ (1 :: Int) .. ]

-- TODO: Make svg chars and import them here. Function will be similar to 'viewGrid'.
viewCoordIndicator :: [Miso.View action]
viewCoordIndicator = []

viewStone :: KnownNat n
          => D.BoardSize           -- ^ boardsize
          -> Int                   -- ^ index of stone in boardvector
          -> G.Stone (G.PlayerN n)
          -> [Miso.View (Operation D.Coord)]
viewStone size i stone = case stone of
                  G.Free -> [ rect_ [ x_ . MisoS.ms $ x - 0.5
                                    , y_ . MisoS.ms $ y - 0.5
                                    , width_ "1"
                                    , height_ "1"
                                    , fillOpacity_ "0"
                                    , onMouseOver $ UpdateAction $ G.Place <$> D.mkCoord size (fromEnum x) (fromEnum y)
                                    , onClick SubmitAction
                                    ] []
                            ]
                  G.Stone p -> [ circle_ [ fill_ (MisoS.ms $ colorize p)
                                         , cx_ $ MisoS.ms x
                                         , cy_ $ MisoS.ms y
                                         , r_ "0.5"
                                         ] []
                               ]
  where y = toEnum $ (i `div` fromEnum size) + 1 :: Double -- TODO: nicer?
        x = toEnum $ (i `mod` fromEnum size) + 1 :: Double

hintStone :: D.Coord -> Miso.View (Operation D.Coord)
hintStone (D.Coord x y) = circle_ [ fill_ "yellow"
                                  , fillOpacity_ "0.5"
                                  , cx_ $ MisoS.ms $ x + 1
                                  , cy_ $ MisoS.ms $ y + 1
                                  , r_ "0.5"
                                  ] []
