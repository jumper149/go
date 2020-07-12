module Lobby.Html ( viewGames
                  , viewCreateButton
                  , editConfig
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

viewGames :: [G.GameId] -> View a
viewGames gs = div_ [] $ fmap viewGame gs

viewGame :: G.GameId -> View a
viewGame g = p_ [] [ a_ [ href_ $ ms url ] [ text name ] ]
  where url = ("/" <>) . toUrlPiece $ safeLink G.apiWrongWS (Proxy :: Proxy G.EndpointGame) g
        name = ms . ("#" <>) . show $ fromEnum g

viewCreateButton :: Bool -> View LobbyOperation
viewCreateButton s = p_ [] [ text "Create a new game with the current configuration: "
                           , button_ [ onClick SubmitConfig
                                     , disabled_ $ not s
                                     ] [ text "Submit"
                                       ]
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
configSelection descr p f a = p_ [] [ text . ms $ (descr <> ": ")
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
