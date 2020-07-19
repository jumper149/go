module Game.Svg ( svgGame
                , viewPassButton
                , viewPlayerChoice
                ) where

import Data.Proxy
import GHC.TypeLits
import qualified Miso.Html as Html
import Miso.String (ms)
import Miso.Svg

import qualified Go.Game.Game as G
import qualified Go.Game.State as G

import Color
import Game.Operation
import Game.Player

svgGame :: [ Html.View (GameOperation b) ] -> Html.View (GameOperation b)
svgGame = svg_ [ viewBox_ "-300 0 1600 1000"
               , onMouseOut $ UpdateAction Nothing
               , Html.class_ "game-app"
               ]

viewPassButton :: Maybe (G.AssociatedAction b) -> [Html.View (GameOperation b)]
viewPassButton mbA = [ rect_ ([ fill_ $ case mbA of
                                          Just G.Pass -> ms $ Red Dark
                                          _ -> ms $ Green Dark
                              , fillOpacity_ "0.7"
                              ] <> rectSizeAttrs) []
                     , text_ [ x_ "1150"
                             , y_ "900"
                             , textAnchor_ "middle"
                             , dominantBaseline_ "middle"
                             , fill_ . ms $ Black Dark
                             , fontSize_ "50pt"
                             ] [ Html.text $ case mbA of
                                               Nothing -> ""
                                               Just G.Pass -> "Pass"
                                               Just (G.Place _) -> "Place"
                               ]
                     , rect_ ([ fillOpacity_ "0"
                              , onMouseOver . UpdateAction $ Just G.Pass
                              , onClick SubmitAction
                              ] <> rectSizeAttrs) []
                     ]
  where rectSizeAttrs = [ x_ "1050"
                        , y_ "50"
                        , width_ "200"
                        , height_ "900"
                        ]

viewPlayerChoice :: G.Game b => Maybe (G.AssociatedPlayer b) -> G.AssociatedPlayer b -> [Html.View (GameOperation b)]
viewPlayerChoice mbP currentP = map viewPlayer [ minBound .. maxBound ]
                             <> hintCurrentPlayer currentP
                             <> hintPlayer mbP
                             <> map clickPlayer [ minBound .. maxBound ]

viewPlayer :: forall b. G.Game b => G.AssociatedPlayer b -> Html.View (GameOperation b)
viewPlayer p = rect_ [ fill_ . ms $ colorize p
                     , x_ "-250"
                     , y_ $ ms y
                     , width_ "200"
                     , height_ $ ms height
                     ] []
  where height = 400 / count
        offSetY = 50
        y = offSetY + height * (toEnum . fromEnum $ p :: Double)
        count = fromIntegral $ natVal (Proxy :: Proxy (G.AssociatedPlayerCount b)) :: Double

clickPlayer :: forall b. G.Game b => G.AssociatedPlayer b -> Html.View (GameOperation b)
clickPlayer p = rect_ [ fillOpacity_ "0"
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

hintCurrentPlayer :: forall b. G.Game b => G.AssociatedPlayer b -> [Html.View (GameOperation b)]
hintCurrentPlayer p = [ circle_ [ fillOpacity_ "0"
                                , stroke_ "grey"
                                , strokeWidth_ "5"
                                , cx_ "-150"
                                , cy_ $ ms y'
                                , r_ "75"
                                ] []
                      ]
  where height = 400 / count
        offSetY = 50
        y = offSetY + height * (toEnum . fromEnum $ p :: Double)
        count = fromIntegral $ natVal (Proxy :: Proxy (G.AssociatedPlayerCount b)) :: Double
        y' = y + height / 2

hintPlayer :: forall b. G.Game b => Maybe (G.AssociatedPlayer b) -> [Html.View (GameOperation b)]
hintPlayer Nothing = []
hintPlayer (Just p) = [ circle_ [ fill_ "grey"
                                , cx_ "-150"
                                , cy_ $ ms y'
                                , r_ "50"
                                ] []
                      ]
  where height = 400 / count
        offSetY = 50
        y = offSetY + height * (toEnum . fromEnum $ p :: Double)
        count = fromIntegral $ natVal (Proxy :: Proxy (G.AssociatedPlayerCount b)) :: Double
        y' = y + height / 2
