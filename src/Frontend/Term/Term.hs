module Frontend.Term.Term ( TermGame ( startTerm
                                     )
                          ) where

import Rules
import GameState

class (Game b c p, Show b, Show p) => TermGame b c p where

  startTerm :: IO (b,p)
  startTerm = start stepTerm endTerm :: IO (b,p)

  stepTerm :: String -> GameState b p -> IO (Action c)
  stepTerm message (GState board player _ _) =
    do putStrLn message
       putStr $ show board
       print player
       readIOSafe $ readAction board

  endTerm :: EndScreen b p -> IO (b,p)
  endTerm endScr = putStrLn str >> return (brd , wnnr)
    where str = show wnnr ++ " wins"
          brd = lastBoard endScr
          wnnr = winner endScr

-- | Decide what and if a string represents an action.
readAction :: forall b c. Board b c => b -> String -> Maybe (Action c)
readAction board str
  | str == "pass" = Just Pass
  | otherwise = Place <$> readCoordOnBoard board str

-- | Read strings from IO, until one is accepted by the reader function.
readIOSafe :: (String -> Maybe a) -> IO a
readIOSafe reader = reader <$> getLine >>= maybe (readIOSafe reader) return
