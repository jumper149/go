module Color ( Color (..)
             , Brightness (..)
             ) where

import GHC.Generics
import Miso.String

data Color = Black   Brightness
           | Red     Brightness
           | Green   Brightness
           | Yellow  Brightness
           | Blue    Brightness
           | Magenta Brightness
           | Cyan    Brightness
           | White   Brightness
  deriving (Eq, Generic, Ord, Read, Show)

data Brightness = Dark
                | Light
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

instance ToMisoString Color where
  toMisoString c = case c of
                     Black   Dark  -> "#282a2e"
                     Black   Light -> "#373b41"

                     Red     Dark  -> "#a54242"
                     Red     Light -> "#cc6666"

                     Green   Dark  -> "#8c9440"
                     Green   Light -> "#b5bd68"

                     Yellow  Dark  -> "#de935f"
                     Yellow  Light -> "#f0c674"

                     Blue    Dark  -> "#5f819d"
                     Blue    Light -> "#81a2be"

                     Magenta Dark  -> "#85678f"
                     Magenta Light -> "#b294bb"

                     Cyan    Dark  -> "#5e8d87"
                     Cyan    Light -> "#8abeb7"

                     White   Dark  -> "#707880"
                     White   Light -> "#c5c8c6"
