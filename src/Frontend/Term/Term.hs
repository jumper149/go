module Frontend.Term.Term ( TermGame (readCoord)
                          ) where

import Control.Monad.Trans

import Game
import State

class (Game b c p, Show b, Show p) => TermGame b c p where

  -- | Decide if a string represents a coordinate and read it.
  readCoord :: b -> String -> Maybe c

  -- | Decide what and if a string represents an action.
  readAction :: b -> String -> Maybe (Action c)
  readAction board str
    | str == "pass" = Just Pass
    | otherwise = Place <$> readCoord board str

instance (TermGame b c p) => MonadPlaying b c p IO where
    getAction = lift . readIOSafe . readAction =<< access currentBoard

    drawGame = do lift . putStr . show =<< access currentBoard
                  lift . print =<< access currentPlayer

    -- TODO more explicit
    drawEnd endScr = putStrLn str >> return ()
      where str = show (winner endScr) ++ " wins"

-- | Read strings from IO, until one is accepted by the reader function.
readIOSafe :: (String -> Maybe a) -> IO a
readIOSafe reader = reader <$> getLine >>= maybe (readIOSafe reader) return
