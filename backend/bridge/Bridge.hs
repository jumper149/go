import Data.Proxy
import Language.PureScript.Bridge

import qualified Go.Board.Default as D
import qualified Go.Board.Loop as L
import Go.Game.Game
import Go.Game.State

types :: [SumType 'Haskell]
types = [ mkSumType (Proxy :: Proxy (GameState D.BoardSquare D.CoordXY D.PlayerBW))
        , mkSumType (Proxy :: Proxy (GameState L.BoardLoop L.CoordXY L.PlayerBW))
        , mkSumType (Proxy :: Proxy (Stone D.PlayerBW))
        , mkSumType (Proxy :: Proxy (Action D.CoordXY))
        , mkSumType (Proxy :: Proxy D.BoardSquare)
        , mkSumType (Proxy :: Proxy D.CoordXY)
        , mkSumType (Proxy :: Proxy D.PlayerBW)
        ]

main :: IO ()
main = writePSTypes frontendPath (buildBridge defaultBridge) types
    where frontendPath = "../frontend/src/Bridge"
