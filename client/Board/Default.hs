module Board.Default ( D.BoardSquare
                     , D.Coord
                     , viewBoard
                     ) where

import qualified Data.Map as M
import Data.Proxy
import qualified Data.Vector.Sized as V
import GHC.TypeLits
import qualified Miso.Html as Html
import qualified Miso.String as MStr
import Miso.String (ms)
import Miso.Svg

import qualified Go.Board.Default as D
import qualified Go.Game.Game as G
import qualified Go.Game.Player as G
import qualified Go.Game.State as G

import Operation
import Player

viewBoard :: forall i n. (KnownNat i, KnownNat n) => D.BoardSquare i n -> Maybe (D.Coord i) -> Html.View (Operation (D.Coord i))
viewBoard (D.BSquare grid) c = svg_ [ Html.style_ $ M.fromList [ ("background-color","grey")
                                                               , ("width","50%")
                                                               ]
                                    , viewBox_ . MStr.unwords . fmap ms $ [ minDim1 , minDim1 , lenDim1 , lenDim1 ]
                                    , onMouseOut $ UpdateAction Nothing
                                    ] $ viewGrid s
                                     <> viewCoordIndicator
                                     <> case c of
                                          Nothing -> []
                                          Just coord -> [ hintStone coord ]
                                     <> (uncurry viewStone <$> zip [ 0 .. ] (concatMap V.toList grid))
  where lenDim1 = toEnum s + margin - minDim1 :: Double
        minDim1 = 1 - margin
        margin = 1.5 -- for coordinate markers with 'viewCoordIndicator'
        s = fromEnum $ natVal (Proxy :: Proxy i)

viewGrid :: Int -> [Html.View action]
viewGrid n = case coordsDim1 of
               [] -> []
               cs -> fmap (\ y -> gridLine (head cs) (last cs) y y) cs
                  <> fmap (\ x -> gridLine x x (head cs) (last cs)) cs
  where gridLine x1 x2 y1 y2 = line_ [ stroke_ "black" , strokeWidth_ "0.1" ,  x1_ x1 , x2_ x2 , y1_ y1 , y2_ y2 ] []
        coordsDim1 = fmap ms . take n $ [ (1 :: Int) .. ]

-- TODO: Make svg chars and import them here. Function will be similar to 'viewGrid'.
viewCoordIndicator :: [Html.View action]
viewCoordIndicator = []

viewStone :: forall i n. (KnownNat i, KnownNat n)
          => Int                   -- ^ index of stone in board
          -> G.Stone (G.PlayerN n)
          -> Html.View (Operation (D.Coord i))
viewStone j stone = case stone of
                      G.Free -> rect_ [ x_ . ms $ x - 0.5
                                      , y_ . ms $ y - 0.5
                                      , width_ "1"
                                      , height_ "1"
                                      , fillOpacity_ "0"
                                      , onMouseOver $ UpdateAction $ G.Place <$> D.mkCoord (fromEnum x) (fromEnum y)
                                      , onClick SubmitAction
                                      ] []
                      G.Stone p -> circle_ [ fill_ (ms $ colorize p)
                                           , cx_ $ ms x
                                           , cy_ $ ms y
                                           , r_ "0.5"
                                           ] []
  where y = toEnum $ D.getY c + 1 :: Double
        x = toEnum $ D.getX c + 1 :: Double
        c = toEnum j :: D.Coord i

hintStone :: (D.Coord i) -> Html.View (Operation (D.Coord i))
hintStone (D.Coord x y) = circle_ [ fill_ "yellow"
                                  , fillOpacity_ "0.5"
                                  , cx_ $ ms $ x + 1
                                  , cy_ $ ms $ y + 1
                                  , r_ "0.5"
                                  ] []
