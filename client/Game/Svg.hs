module Game.Svg ( svgGame
                , viewPassButton
                , viewPlayerChoice
                ) where

import qualified Data.Map as M
import Data.Proxy
import GHC.TypeLits
import qualified Miso.Html as Html
import Miso.String (ms)
import Miso.Svg

import qualified Go.Game.Game as G
import qualified Go.Game.State as G

import Game.Operation
import Game.Player

svgGame :: [ Html.View (GameOperation b) ] -> Html.View (GameOperation b)
svgGame = svg_ [ viewBox_ "-300 0 1600 1000"
               , onMouseOut $ UpdateAction Nothing
               , Html.class_ "game-app"
               ]

viewPassButton :: Maybe (G.AssociatedAction b) -> [Html.View (GameOperation b)]
viewPassButton mbA = [ rect_ ([ fill_ $ case mbA of
                                          Just G.Pass -> "red"
                                          _ -> "yellow"
                              , fillOpacity_ "0.5"
                              ] <> rectSizeAttrs) []
--                     , text_ [ x_ "50"
--                             , y_ "25"
--                             , textAnchor_ "middle"
--                             , dominantBaseline_ "middle"
--                             , fill_ "black"
--                             ] [ Html.text $ case mbA of
--                                               Nothing -> ""
--                                               Just G.Pass -> "Pass"
--                                               Just (G.Place _) -> "Place"
--                               ]
                     , rect_ ([ fillOpacity_ "0"
                              , onMouseOver $ UpdateAction $ Just G.Pass
                              , onClick $ SubmitAction
                              ] <> rectSizeAttrs) []
                     ]
  where rectSizeAttrs = [ x_ "1050"
                        , y_ "50"
                        , width_ "200"
                        , height_ "900"
                        ]

viewPlayerChoice :: G.Game b => Maybe (G.AssociatedPlayer b) -> [Html.View (GameOperation b)]
viewPlayerChoice mbP = [ rect_ [ fillOpacity_ "0"
                               , x_ "10"
                               , y_ "10"
                               , width_ "80"
                               , height_ "15"
                               , onClick $ SubmitPlayer Nothing
                               ] []
                       ] <> map viewPlayer [ minBound .. maxBound ]
                         <> hintPlayer mbP

viewPlayer :: forall b. G.Game b => G.AssociatedPlayer b -> Html.View (GameOperation b)
viewPlayer p = rect_ [ fill_ . ms $ colorize p
                     , x_ "-250"
                     , y_ $ ms y
                     , width_ "200"
                     , height_ $ ms height
                     , onClick $ SubmitPlayer $ Just p
                     ] []
  where height = 400 / count
        offSetY = 50
        y = offSetY + height * (toEnum . fromEnum $ p :: Double)
        count = fromIntegral $ natVal (Proxy :: Proxy (G.AssociatedPlayerCount b)) :: Double

hintPlayer :: forall b. G.Game b => Maybe (G.AssociatedPlayer b) -> [Html.View (GameOperation b)]
hintPlayer Nothing = []
hintPlayer (Just p) = [ circle_ [ fill_ "red"
                                , cx_ "-150"
                                , cy_ $ ms y'
                                , r_ "100"
                                ] []
                      ]
  where height = 400 / count
        offSetY = 50
        y = offSetY + height * (toEnum . fromEnum $ p :: Double)
        count = fromIntegral $ natVal (Proxy :: Proxy (G.AssociatedPlayerCount b)) :: Double
        y' = y + height / 2
