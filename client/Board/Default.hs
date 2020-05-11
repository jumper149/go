module Board.Default ( D.Board
                     , D.Coord
                     , viewBoard
                     ) where

import Data.Bifunctor
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

viewBoard :: forall i n. (KnownNat i, KnownNat n) => D.Board i n -> Maybe (D.Coord i) -> Html.View (Operation (D.Coord i))
viewBoard (D.Board grid) c = svg_ [ Html.style_ $ M.fromList [ ("background-color","grey")
                                                             , ("width","50%")
                                                             ]
                                  , viewBox_ . MStr.unwords . fmap ms $ [ minDim1 , minDim1 , lenDim1 , lenDim1 ]
                                  , onMouseOut $ UpdateAction Nothing
                                  ] $ viewGrid s
                                   <> viewCoordIndicator
                                   <> case c of
                                        Nothing -> []
                                        Just coord -> [ hintStone coord ]
                                   <> (uncurry viewStone <$> zip [ minBound .. maxBound ] (concatMap V.toList grid))
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
          => D.Coord i             -- ^ index of stone in board
          -> G.Stone (G.PlayerN n)
          -> Html.View (Operation (D.Coord i))
viewStone c stone = case stone of
                      G.Free -> rect_ [ x_ . ms $ x - 0.5
                                      , y_ . ms $ y - 0.5
                                      , width_ "1"
                                      , height_ "1"
                                      , fillOpacity_ "0"
                                      , onMouseOver $ UpdateAction $ G.Place <$> Just c
                                      , onClick SubmitAction
                                      ] []
                      G.Stone p -> circle_ [ fill_ (ms $ colorize p)
                                           , cx_ $ ms x
                                           , cy_ $ ms y
                                           , r_ "0.5"
                                           ] []
  where (x,y) = let cast = fromIntegral . (+1) :: Integer -> Double
                in bimap cast cast $ D.getCoord c

hintStone :: KnownNat i => D.Coord i -> Html.View action
hintStone c = circle_ [ fill_ "yellow"
                      , fillOpacity_ "0.5"
                      , cx_ $ ms $ fromEnum x + 1
                      , cy_ $ ms $ fromEnum y + 1
                      , r_ "0.5"
                      ] []
  where (x,y) = D.getCoord c
