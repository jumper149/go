{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Frontend.Term.Term ( TermGame ( display
                                     , startTerm
                                     )
                          ) where

import Rules
import GameState

class Game b c p => TermGame b c p where

  display :: b -> p -> String

  startTerm :: IO (b,p)
  startTerm = start stepTerm :: IO (b,p)

  stepTerm :: GameState b p -> IO (b,p)
  stepTerm (GState board player oldBoard passes) =
    do putStr $ display board player
       action <- readIOSafe $ readAction board
       endState <- step stepTerm (GState board player oldBoard passes) action
       endTerm endState

  endTerm :: (b,p) -> IO (b,p)
  endTerm (board , player) = putStr (display board player) >> return (board , player)

-- | Decide what and if a string represents an action.
readAction :: forall b c. Board b c => b -> String -> Maybe (Action c)
readAction board str
  | str == "pass" = Just Pass
  | otherwise = Place <$> readCoordOnBoard board str

-- | Read strings from IO, until one is accepted by the reader function.
readIOSafe :: (String -> Maybe a) -> IO a
readIOSafe reader = reader <$> getLine >>= maybe (readIOSafe reader) return
