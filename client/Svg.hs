module Svg ( viewPassButton
           ) where

import qualified Data.Map as M
import qualified Miso.Html as Html
import Miso.Svg

import qualified Go.Game.State as G

import Operation

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
