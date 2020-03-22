module Frontend.Term.Term ( TermGame (readCoord)
                          , game
                          ) where

import Control.Monad.State.Strict

import Data.Either (fromRight)

import Game
import Rules
import State

class (Game b c p, Show b, Show p) => TermGame b c p where

  -- | Decide if a string represents a coordinate and read it.
  readCoord :: b -> String -> Maybe c

  -- | Decide what and if a string represents an action.
  readAction :: b -> String -> Maybe (Action c)
  readAction board str
    | str == "pass" = Just Pass
    | otherwise = Place <$> readCoord board str

game :: (TermGame b c p) => Rules -> IO (EndScreen b p)
game rules = finalizeState . fromRight undefined <$> runPlayingT rules (play action render)

action :: (TermGame b c p) => PlayingT b c p IO (Action c)
action = lift . readIOSafe . readAction =<< gets currentBoard

render :: (TermGame b c p) => PlayingT b c p IO ()
render = do lift . putStr . show =<< gets currentBoard
            lift . print =<< gets currentPlayer

-- | Read strings from IO, until one is accepted by the reader function.
readIOSafe :: (String -> Maybe a) -> IO a
readIOSafe reader = reader <$> getLine >>= maybe (readIOSafe reader) return
