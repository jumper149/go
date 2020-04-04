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
action :: (TermGame b c p, MonadIO m) => GameState b c p -> m (Either Exception (Action c))
action gs = do str <- liftIO getLine
               let mbActn = case str of
                              "pass" -> Just Pass
                              _ -> Place <$> readCoord (currentBoard gs) str
               case mbActn of
                 Just actn -> return . Right $ actn
                 Nothing -> return $ throwError ExceptRedo

-- | Print board and other information to stdout.
render :: (TermGame b c p, MonadIO m) => GameState b c p -> m ()
render gs = do liftIO . putStr . show $ currentBoard gs
               liftIO . print $ currentPlayer gs

-- | Play a whole game in the terminal.
game :: TermGame b c p => Config -> IO (EndScreen b p)
game config = do eithGs <- runConfiguredT config $ playPlayingT $ play action render
                 return $ either (error . show) finalizeState eithGs
