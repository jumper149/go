module Lobby.Svg ( viewCreateButton
                 ) where

import qualified Data.Map as M
import qualified Miso.Html as Html
import Miso.Svg

import Lobby.Operation

viewCreateButton :: Html.View LobbyOperation
viewCreateButton = svg_ [ Html.style_ $ M.fromList [ ("background-color","grey")
                                                   , ("width","50%")
                                                   , ("align-content","center")
                                                   ]
                        , viewBox_ "0 0 100 50"
                        ] [ rect_ ([ fill_ "blue"
                                   , fillOpacity_ "0.5"
                                   ] <> rectSizeAttrs) []
                          , text_ [ x_ "50"
                                  , y_ "25"
                                  , textAnchor_ "middle"
                                  , dominantBaseline_ "middle"
                                  , fill_ "black"
                                  ] [ Html.text "Submit Config"
                                    ]
                          , rect_ ([ fillOpacity_ "0"
                                   , onClick $ SubmitConfig
                                   ] <> rectSizeAttrs) []
                          ]
  where rectSizeAttrs = [ x_ "10"
                        , y_ "10"
                        , width_ "80"
                        , height_ "30"
                        ]
