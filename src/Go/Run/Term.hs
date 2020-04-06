module Go.Run.Term ( TermGame ( renderBoard
                              , readCoord
                              )
                   , game
                   , action
                   , render
                   , renderStone
                   ) where

import Control.Monad.Except

import Go.Config
import Go.Game.End
import Go.Game.Game
import Go.Game.Playing
import Go.Game.State

class Game b c n => TermGame b c n where
    -- | Display a board on multiple lines.
    renderBoard :: b -> String

    -- | Decide whether a string represents a coordinate and read it.
    readCoord :: b -> String -> Maybe c

-- | Get action from 'IO' and check sanity.
action :: (TermGame b c n, MonadIO m) => GameState b c n -> m (Either Exception (Action c))
action gs = do str <- liftIO getLine
               let mbActn = case str of
                              "pass" -> Just Pass
                              _ -> Place <$> readCoord (currentBoard gs) str
               case mbActn of
                 Just actn -> return . Right $ actn
                 Nothing -> return $ throwError ExceptRedo

-- | Print board and other information to stdout.
render :: (TermGame b c n, MonadIO m) => GameState b c n -> m ()
render gs = do liftIO . putStr . renderBoard $ currentBoard gs
               liftIO . print $ currentPlayer gs

-- | Play a whole game in the terminal.
game :: TermGame b c n => Config -> IO (EndScreen b n)
game config = do gs <- either (error . show) id <$> runConfiguredT config initState
                 fmap finalizeState $ execPlayingT (rules config) gs $ play action render

-- | Render a stone as a single character string.
renderStone :: Enum p => Stone p -> Char
renderStone Free = ' '
renderStone (Stone p) = head . show $ fromEnum p
