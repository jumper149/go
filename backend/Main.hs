import qualified Go.Board.Default as D
import Go.Game.End (EndScreen)
import Go.Game.Config
import Go.Run.Server.JSON

import Control.Monad (void)

main :: IO ()
main = void (serverJSON def :: IO (EndScreen D.BoardSquare D.PlayerBW))
