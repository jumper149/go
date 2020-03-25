import Data.Proxy
import Language.PureScript.Bridge
import Language.PureScript.Bridge.TypeParameters

import qualified Go.Board.Default as D
import qualified Go.Board.Loop as L
import Go.Game.Game
import Go.Game.State

types :: [SumType 'Haskell]
types = [ mkSumType (Proxy :: Proxy (GameState B C P))
        , mkSumType (Proxy :: Proxy (Stone P))
        , mkSumType (Proxy :: Proxy (Action C))

        , mkSumType (Proxy :: Proxy D.BoardSquare)
        , mkSumType (Proxy :: Proxy D.CoordXY)
        , mkSumType (Proxy :: Proxy D.PlayerBW)

        , mkSumType (Proxy :: Proxy L.BoardLoop)
        , mkSumType (Proxy :: Proxy L.CoordXY)
        , mkSumType (Proxy :: Proxy L.PlayerBW)
        ]

main :: IO ()
main = writePSTypes frontendPath (buildBridge defaultBridge) types
    where frontendPath = "../frontend/src"
