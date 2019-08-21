{-# LANGUAGE ScopedTypeVariables #-}

module Frontend.Term.Term ( TermGame ( startTerm
                                     )
                          ) where

import Rules
import GameState

import Data.List ( sortOn
                 )

class (Game b c p, Show b, Show p) => TermGame b c p where

  startTerm :: IO (b,p)
  startTerm = start stepTerm endTerm :: IO (b,p)

  stepTerm :: GameState b p -> IO (GameState b p)
  stepTerm (GState board player oldBoard passes) =
    do putStr $ show board
       putStr $ show player ++ "\n"
       action <- readIOSafe $ readAction board
       step stepTerm (GState board player oldBoard passes) action

  endTerm :: EndScreen b p -> IO (b,p)
  endTerm endScr = putStr str >> return (brd , winner)
    where str = show winner ++ " wins\n"
          brd = board endScr
          winner = fst . last $ sortOn snd $ points endScr

-- | Decide what and if a string represents an action.
readAction :: forall b c. Board b c => b -> String -> Maybe (Action c)
readAction board str
  | str == "pass" = Just Pass
  | otherwise = Place <$> readCoordOnBoard board str

-- | Read strings from IO, until one is accepted by the reader function.
readIOSafe :: (String -> Maybe a) -> IO a
readIOSafe reader = reader <$> getLine >>= maybe (readIOSafe reader) return
