module Game.Svg ( viewPassButton
                , viewPlayerChoice
                ) where

import qualified Data.Map as M
import GHC.TypeLits
import qualified Miso.Html as Html
import Miso.String (ms)
import Miso.Svg

import qualified Go.Game.Player as G
import qualified Go.Game.State as G

import Game.Operation
import Game.Player

viewPassButton :: Maybe (G.Action c) -> Html.View (GameOperation b c n)
viewPassButton mbA = svg_ [ Html.style_ $ M.fromList [ ("background-color","grey")
                                                     , ("width","50%")
                                                     ]
                          , viewBox_ "0 0 100 50"
                          , onMouseOut $ UpdateAction Nothing
                          ] [ rect_ ([ fill_ $ case mbA of
                                                 Just G.Pass -> "red"
                                                 _ -> "yellow"
                                     , fillOpacity_ "0.5"
                                     ] <> rectSizeAttrs) []
                            , text_ [ x_ "50"
                                    , y_ "25"
                                    , textAnchor_ "middle"
                                    , dominantBaseline_ "middle"
                                    , fill_ "black"
                                    ] [ Html.text $ case mbA of
                                                      Nothing -> ""
                                                      Just G.Pass -> "Pass"
                                                      Just (G.Place _) -> "Place"
                                      ]
                            , rect_ ([ fillOpacity_ "0"
                                     , onMouseOver $ UpdateAction $ Just G.Pass
                                     , onClick $ SubmitAction
                                     ] <> rectSizeAttrs) []
                            ]
  where rectSizeAttrs = [ x_ "10"
                        , y_ "10"
                        , width_ "80"
                        , height_ "30"
                        ]

viewPlayerChoice :: KnownNat n => Maybe (G.PlayerN n) -> Html.View (GameOperation b c n)
viewPlayerChoice mbP = svg_ [ Html.style_ $ M.fromList [ ("background-color","grey")
                                                       , ("width","20%")
                                                       ]
                            , viewBox_ "0 0 100 50"
                            ] ([ rect_ [ fillOpacity_ "0"
                                       , x_ "10"
                                       , y_ "10"
                                       , width_ "80"
                                       , height_ "15"
                                       , onClick $ SubmitPlayer Nothing
                                       ] []
                               ] <> map viewPlayer [ minBound .. maxBound ]
                                 <> [ hintPlayer mbP ])

viewPlayer :: forall b c n. KnownNat n => G.PlayerN n -> Html.View (GameOperation b c n)
viewPlayer p = rect_ [ fill_ . ms $ colorize p
                     , x_ $ ms x
                     , y_ "25"
                     , width_ $ ms width
                     , height_ "15"
                     , onClick $ SubmitPlayer $ Just p
                     ] []
  where width = 80 / count
        count = toEnum $ G.countPlayers (toEnum 0 :: G.PlayerN n) :: Double
        x = 10 + 80 * (toEnum . fromEnum $ p :: Double) / count

hintPlayer :: forall b c n. KnownNat n => Maybe (G.PlayerN n) -> Html.View (GameOperation b c n)
hintPlayer Nothing = circle_ [ fill_ "black"
                             , cx_ "82.5"
                             , cy_ "17.5"
                             , r_ "5"
                             ] []
hintPlayer (Just p) = circle_ [ fill_ "red"
                              , cx_ $ ms x
                              , cy_ "32.5"
                              , r_ "5"
                              ] []
  where count = toEnum $ G.countPlayers (toEnum 0 :: G.PlayerN n) :: Double
        x = 10 + 80 * (toEnum . fromEnum $ p :: Double) / count + 80 / (2 * count)
