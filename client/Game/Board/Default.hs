module Game.Board.Default ( D.Board
                          , D.Coord
                          , viewBoard
                          ) where

import Data.Bifunctor
import Data.Proxy
import qualified Data.Vector.Sized as V
import GHC.TypeLits
import qualified Miso.Html as Html
import Miso.String (ms)
import Miso.Svg

import qualified Go.Board.Default as D
import qualified Go.Game.Game as G
import qualified Go.Game.Player as G
import qualified Go.Game.State as G

import Color
import Game.Operation
import Game.Player

xOffset :: Double
xOffset = 100

xLength :: Double
xLength = 800

viewBoard :: forall i n. (KnownNat i, KnownNat n) => D.Board i n -> Maybe (D.Coord i) -> [Html.View (GameOperation (D.Board i n))]
viewBoard (D.Board grid) c = viewGrid s
                          <> viewCoordIndicator
                          <> case c of
                               Nothing -> []
                               Just coord -> [ hintStone s coord ]
                          <> (uncurry (viewStone s) <$> zip [ minBound .. maxBound ] (concatMap V.toList grid))
  where s = fromEnum $ natVal (Proxy :: Proxy i)

viewGrid :: Int -> [Html.View action]
viewGrid s = case xCoords of
               [] -> []
               _ -> fmap (\ y -> gridLine (head xCoords - strokeWidth/2) (last xCoords + strokeWidth/2) y y) xCoords -- pattern matching to make head and tail safe
                 <> fmap (\ x -> gridLine x x (head xCoords - strokeWidth/2) (last xCoords + strokeWidth/2)) xCoords
  where gridLine x1 x2 y1 y2 = line_ [ stroke_ . ms $ Black Dark
                                     , strokeWidth_ $ ms strokeWidth
                                     , x1_ $ ms x1
                                     , x2_ $ ms x2
                                     , y1_ $ ms y1
                                     , y2_ $ ms y2
                                     ] []
        xCoords = take s $ (+ xOffset) <$> (* (xLength / n)) <$> toEnum <$> [ (0 :: Int) .. ] :: [Double]
        n = fromIntegral s - 1
        strokeWidth = 5 :: Double

-- TODO: Make svg chars and import them here. Function will be similar to 'viewGrid'.
viewCoordIndicator :: [Html.View action]
viewCoordIndicator = []

viewStone :: forall i n. (KnownNat i, KnownNat n)
          => Int
          -> D.Coord i             -- ^ index of stone in board
          -> G.Stone (G.PlayerN n)
          -> Html.View (GameOperation (D.Board i n))
viewStone s c stone = case stone of
                        G.Free -> rect_ [ x_ . ms $ x - xLength/(2*n)
                                        , y_ . ms $ y - xLength/(2*n)
                                        , width_ . ms $ xLength/n
                                        , height_ . ms $ xLength/n
                                        , fillOpacity_ "0"
                                        , onMouseOver $ UpdateAction $ G.Place <$> Just c
                                        , onClick $ SubmitAction
                                        ] []
                        G.Stone p -> circle_ [ fill_ (ms $ colorize p)
                                             , cx_ $ ms x
                                             , cy_ $ ms y
                                             , r_ . ms $ xLength/(2*n)
                                             ] []
  where (x,y) = let correctVal = (+ xOffset) . (* (xLength/n)) . fromIntegral :: Integer -> Double
                in bimap correctVal correctVal $ D.getCoord c
        n = fromIntegral s - 1

hintStone :: KnownNat i => Int -> D.Coord i -> Html.View action
hintStone s c = circle_ [ fill_ . ms $ Green Light
                        , fillOpacity_ "0.7"
                        , cx_ $ ms $ fromEnum x + 1
                        , cy_ $ ms $ fromEnum y + 1
                        , r_ . ms $ xLength/(2*n)
                        ] []
  where (x,y) = let correctVal = (+ xOffset) . (* (xLength/n)) . fromIntegral :: Integer -> Double
                in bimap correctVal correctVal $ D.getCoord c
        n = fromIntegral s - 1
