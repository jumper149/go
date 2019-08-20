{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Frontend.Term.Term ( TermGame ( startTerm
                                     )
                          ) where

import Rules
import GameState

class (Game b c p, Show b, Show p) => TermGame b c p where

  startTerm :: IO (b,p)
  startTerm = start stepTerm :: IO (b,p)

  stepTerm :: GameState b p -> IO (b,p)
  stepTerm (GState board player oldBoard passes) =
    do putStr $ show board
       putStr $ show player
       action <- readIOSafe $ readAction board
       endState <- step stepTerm (GState board player oldBoard passes) action
       endTerm endState

  endTerm :: (b,p) -> IO (b,p)
  endTerm (board , player) = putStr str >> return (board , player)
    where str = show player ++ " wins\n"

-- | Decide what and if a string represents an action.
readAction :: forall b c. Board b c => b -> String -> Maybe (Action c)
readAction board str
  | str == "pass" = Just Pass
  | otherwise = Place <$> readCoordOnBoard board str

-- | Read strings from IO, until one is accepted by the reader function.
readIOSafe :: (String -> Maybe a) -> IO a
readIOSafe reader = reader <$> getLine >>= maybe (readIOSafe reader) return
