module Go.Run.Term ( TermGame (readCoord)
                   , game
                   , action
                   , render
                   ) where

import Control.Monad.Except

import Go.Game.Config
import Go.Game.End
import Go.Game.Game
import Go.Game.Playing
import Go.Game.State

class (Game b c p, Show b, Show p) => TermGame b c p where

    -- | Decide whether a string represents a coordinate and read it.
    readCoord :: b -> String -> Maybe c

-- | Get action from 'IO' and check sanity.
action :: TermGame b c p => GameState b c p -> IO (Either Exception (Action c))
action gs = do str <- getLine
               let mbActn = case str of
                              "pass" -> Just Pass
                              _ -> Place <$> readCoord (currentBoard gs) str
               case mbActn of
                 Just actn -> return . return $ actn
                 Nothing -> return $ throwError ExceptRedo

-- | Print board and other information to stdout.
render :: TermGame b c p => GameState b c p -> IO ()
render gs = do putStr . show $ currentBoard gs
               print $ currentPlayer gs

-- | Play a whole game in the terminal.
game :: TermGame b c p => Config -> IO (EndScreen b p)
game config = finalizeState <$> playPlayingT config (play action render)
