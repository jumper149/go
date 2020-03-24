module Go.Frontend.Term ( TermGame (readCoord)
                        , game
                        , action
                        , render
                        ) where

import Control.Monad.Except
import Control.Monad.State.Strict

import Data.Either (fromRight)

import Go.Game.End
import Go.Game.Game
import Go.Game.Playing
import Go.Game.Rules
import Go.Game.State

class (Game b c p, Show b, Show p) => TermGame b c p where

    -- | Decide whether a string represents a coordinate and read it.
    readCoord :: b -> String -> Maybe c

-- | Decide what and if a string represents an action.
readAction :: TermGame b c p => b -> IO (Maybe (Action c))
readAction board = do str <- getLine
                      case str of
                        "pass" -> return $ Just Pass
                        _ -> return $ Place <$> readCoord board str

-- | Get action from 'IO' and lift it.
action :: TermGame b c p => GameState b c p -> IO (Either Exception (Action c))
action gs = do str <- getLine
               let mbActn = case str of
                              "pass" -> Just Pass
                              _ -> Place <$> readCoord (currentBoard gs) str
               case mbActn of
                 Just actn -> return . return $ actn
                 Nothing -> return $ throwError ExceptRedo -- TODO ExceptRedo stops the running program, dont know why

-- | Print board and other information to stdout.
render :: TermGame b c p => GameState b c p -> IO ()
render gs = do putStr . show $ currentBoard gs
               print $ currentPlayer gs

-- | Play a whole game in the terminal.
game :: TermGame b c p => Rules -> IO (EndScreen b p)
game rules = finalizeState <$> runPlayingT rules (play action render)
