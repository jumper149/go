module Representation ( viewGameModelRep -- TODO: reexport GameModelRep and GameOperationRep when all case-handling is done here
                      ) where

import Miso.Html

import Game.Model
import Representation.Model
import Representation.Operation

viewGameModelRep :: GameModelRep -> View GameOperationRep
viewGameModelRep model = case model of
                           GameModelD_9_2  m -> GameOperationD_9_2  <$> viewGameModel m
                           GameModelD_13_2 m -> GameOperationD_13_2 <$> viewGameModel m
