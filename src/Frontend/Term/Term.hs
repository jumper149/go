module Frontend.Term.Term ( TermGame (..)
                          , showStone
                          ) where

import Game
import State

class (Game b c p, Show b, Show p) => TermGame b c p where

  startTerm :: IO (b,p)
  startTerm = start stepTerm endTerm :: IO (b,p)

  stepTerm :: GameState b p -> IO (Action c)
  stepTerm state =
    do putStrLn $ messageOnPrev state
       putStr . show $ currBoard state
       print $ currPlayer state
       readIOSafe . readAction $ currBoard state

  endTerm :: EndScreen b p -> IO (b,p)
  endTerm endScr = putStrLn str >> return (lastBoard endScr , winner endScr)
    where str = show (winner endScr) ++ " wins"

-- | Decide what and if a string represents an action.
readAction :: forall b c. Board b c => b -> String -> Maybe (Action c)
readAction board str
  | str == "pass" = Just Pass
  | otherwise = Place <$> readCoord board str

-- | Read strings from IO, until one is accepted by the reader function.
readIOSafe :: (String -> Maybe a) -> IO a
readIOSafe reader = reader <$> getLine >>= maybe (readIOSafe reader) return

-- | Show a stone as a single character string.
showStone :: forall p. Player p => Stone p -> String
showStone Free = " "
showStone (Stone p) = [ char p ]
