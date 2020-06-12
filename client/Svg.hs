module Svg ( viewPassButton
           , viewPlayerChoice
           ) where

import qualified Data.Map as M
import GHC.TypeLits
import qualified Miso.Html as Html
import Miso.String (ms)
import Miso.Svg

import qualified Go.Game.Player as G
import qualified Go.Game.State as G

import Operation
import Player

viewPassButton :: Maybe (G.Action c) -> Html.View (Operation b c n)
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
                                     , onClick SubmitAction
                                     ] <> rectSizeAttrs) []
                            ]
  where rectSizeAttrs = [ x_ "10"
                        , y_ "10"
                        , width_ "80"
                        , height_ "30"
                        ]

viewPlayerChoice :: KnownNat n => Maybe (G.PlayerN n) -> Html.View (Operation b c n)
viewPlayerChoice mbP = svg_ [ Html.style_ $ M.fromList [ ("background-color","grey")
                                                       , ("width","20%")
                                                       ]
                            , viewBox_ "0 0 100 50"
                            ] ([ rect_ [ fill_ "blue"
                                       , x_ "10"
                                       , y_ "10"
                                       , width_ "80"
                                       , height_ "15"
                                       , onClick $ SubmitPlayer Nothing
                                       ] []
                               ] <> map viewPlayer [ minBound .. maxBound ])

viewPlayer :: forall b c n. KnownNat n => G.PlayerN n -> Html.View (Operation b c n)
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
