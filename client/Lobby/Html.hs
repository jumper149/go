module Lobby.Html ( viewHeader
                  , viewGames
                  , viewConfig
                  , viewFooter
                  ) where

import Data.Aeson
import Data.Default.Class
import Data.Proxy
import Miso.Html
import Miso.String
import Servant.API

import qualified Go.Config as G
import qualified Go.Game.Rules as G
import qualified Go.Run.API as G
import qualified Go.Run.GameId as G

import Lobby.Operation

viewHeader :: View a
viewHeader = div_ [ class_ "big-header"
                  ] [ h1_ [] [ text "go" ]
                    , h2_ [] [ text "a traditional game with a modern approach" ]
                    ]

viewGames :: [G.GameId] -> View a
viewGames gs = div_ [ class_ "lobby-body" ] $ [ h1_ [] [ text "join existing game" ]
                         ] <> fmap viewGame gs

viewGame :: G.GameId -> View a
viewGame g = a_ [ href_ $ ms url
                ] [ div_ [ class_ "game"
                         ] [ text name ]
                  ]
  where url = ("/" <>) . toUrlPiece $ safeLink G.apiWrongWS (Proxy :: Proxy G.EndpointGame) g
        name = ms . ("#" <>) . show $ fromEnum g

viewConfig :: Bool -- ^ enable submit-button
           -> View LobbyOperation
viewConfig s = div_ [ class_ "lobby-body"
                    ] [ h1_ [] [ text "configure a new game" ]
                      , editConfig
                      , viewCreateButton s
                      ]

viewCreateButton :: Bool -> View LobbyOperation
viewCreateButton s = button_ [ onClick SubmitConfig
                             , disabled_ $ not s
                             ] [ text "create a new game"
                               ]

editConfig :: View LobbyOperation
editConfig = p_ [] [ configSelection "Board"   show              SetConfigBoard                          $ G.board def
                   , configSelection "Size"    (show . fromEnum) SetConfigSize                           $ G.size def
                   , configSelection "Players" (show . fromEnum) SetConfigPlayers                        $ G.players def
                   , configSelection "Ko"      show              (SetConfigRules . SetConfigRuleKo)      $ G.ko . G.ruleset $ def
                   , configSelection "Suicide" show              (SetConfigRules . SetConfigRuleSuicide) $ G.suicide . G.ruleset $ def
                   , configSelection "Passing" show              (SetConfigRules . SetConfigRulePassing) $ G.passing . G.ruleset $ def
                   ]

configSelection :: forall a. (Bounded a, Enum a, Eq a, FromJSON a, ToJSON a)
                => String
                -> (a -> String)
                -> (a -> SetConfig)
                -> a
                -> View LobbyOperation
configSelection descr p f a = div_ [ class_ "config-option"
                                   ] [ h2_ [] [ text . ms $ descr ]
                                     , fmap (SetConfig . f) $ selection p a
                                     ]

selection :: forall a. (Bounded a, Enum a, Eq a, FromJSON a, ToJSON a)
          => (a -> String)
          -> a
          -> View a
selection p a = select_ [ onInput recoverOption ] $ fmap option [ minBound .. maxBound @a ]
  where option b = option_ (addDefaultAttr b [ value_ . toMisoString $ encode b ]) [ text . toMisoString $ p b ]
        addDefaultAttr b attrs = if b == a
                                    then (selected_ True) : attrs
                                    else attrs
        recoverOption mstr = case decode <$> fromMisoStringEither mstr of
                               Right (Just b) -> b
                               _ -> a

viewFooter :: View a
viewFooter = div_ [ class_ "footer"
                  ] [ div_ [ class_ "footer-content"
                           ] [ p_ [] [ strong_ [] [ text $ "go" ]
                                     , text " by "
                                     , a_ [ href_ "https://github.com/jumper149"
                                          ] [ text "jumper149" ]
                                     , text ". BSD-3 licensed."
                                     ]
                             , p_ [] [ text "You can find the source code "
                                     , a_ [ href_ "https://github.com/jumper149/go"
                                          ] [ text "here" ]
                                     , "."
                                     ]
                             ]
                    ]
