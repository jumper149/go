module Main where

import Frontend.Term.Term
import qualified Board.Default as D
import qualified Board.Loop as L

import System.Console.GetOpt
import System.Environment ( getArgs
                          )
import Control.Monad ( void
                     )

data Interface = Term
  deriving (Read, Enum)

data Board = Default
           | Loop
  deriving (Read, Enum)

data Options = Options { optInterface :: Interface
                       , optBoard :: Board
                       }

defaultOptions :: Options
defaultOptions = Options { optInterface = Term
                         , optBoard = Default
                         }

options :: [OptDescr (Options -> IO Options)]
options = [ Option ['i'] ["interface"]
              (ReqArg
                (\ arg opt -> return opt { optInterface = read arg :: Interface })
                "Interface")
              "Interface"
          , Option ['b'] ["board"]
              (ReqArg
                (\ arg opt -> return opt { optBoard = read arg :: Board })
                "Board")
              "Board"
          ]

choose :: (Interface,Board) -> IO ()
choose (Term , Default) = void (startTerm :: IO (D.BoardSquare , D.PlayerBW))
choose (Term , Loop) = void (startTerm :: IO (L.BoardLoop , L.PlayerBW))

main :: IO ()
main = do args <- getArgs

          let (optArgs , _ , _) = getOpt Permute options args

          opts <- foldl (>>=) (return defaultOptions) optArgs

          let Options { optInterface = interface
                      , optBoard = board
                      } = opts

          choose (interface , board)
