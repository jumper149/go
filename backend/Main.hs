import qualified Go.Board.Default as D
import Go.Game.End (EndScreen)
import Go.Game.Rules (defaultRules)
import Go.Run.Server.JSON

import Control.Monad (void)

main :: IO ()
main = void (serverJSON defaultRules :: IO (EndScreen D.BoardSquare D.PlayerBW))
