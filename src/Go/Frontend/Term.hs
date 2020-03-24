module Go.Frontend.Term ( TermGame (readCoord)
                        , game
                        , action'
                        , render'
                        ) where

import Control.Monad.State.Strict

import Data.Either (fromRight)

import Go.Game.End
import Go.Game.Game
import Go.Game.Rules
import Go.Game.State

action' :: TermGame b c p => b -> IO (Action c)
action' = readIOSafe . readAction

render' :: TermGame b c p => b -> p -> IO ()
render' board player = do putStr . show $ board
                          print player

class (Game b c p, Show b, Show p) => TermGame b c p where

    -- | Decide whether a string represents a coordinate and read it.
    readCoord :: b -> String -> Maybe c

-- | Decide what and if a string represents an action.
readAction :: TermGame b c p => b -> String -> Maybe (Action c)
readAction board str
  | str == "pass" = Just Pass
  | otherwise = Place <$> readCoord board str

-- | Read strings from 'IO', until one is accepted by the reader function.
readIOSafe :: (String -> Maybe a) -> IO a
readIOSafe reader = reader <$> getLine >>= maybe (readIOSafe reader) return

-- | Get action from 'IO' and lift it.
action :: TermGame b c p => PlayingT b c p IO (Action c)
action = lift . readIOSafe . readAction =<< gets currentBoard

-- | Print board and other information to stdout.
render :: TermGame b c p => PlayingT b c p IO ()
render = do lift . putStr . show =<< gets currentBoard
            lift . print =<< gets currentPlayer

-- | Play a whole game in the terminal.
game :: TermGame b c p => Rules -> IO (EndScreen b p)
game rules = finalizeState . fromRight undefined <$> runPlayingT rules (play action render)
