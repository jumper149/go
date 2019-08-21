module Main where

import Frontend.Term.Term
import qualified Board.Default as D
import qualified Board.Loop as L

import System.Console.GetOpt
import System.Environment ( getArgs
                          )
import Control.Monad ( void
                     )

data Interfaces = Term
  deriving Read

data Boards = Default
            | Loop
  deriving Read

data Options = Options { optInterface :: Interfaces
                       , optBoard :: Boards
                       }

defaultOptions :: Options
defaultOptions = Options { optInterface = Term
                         , optBoard = Default
                         }

options :: [OptDescr (Options -> IO Options)]
options = [ Option ['i'] ["interface"]
              (ReqArg
                (\ arg opt -> return opt { optInterface = read arg :: Interfaces })
                "Interface")
              "Interface"
          , Option ['b'] ["board"]
              (ReqArg
                (\ arg opt -> return opt { optBoard = read arg :: Boards })
                "Board")
              "Board"
          ]

choose :: (Interfaces,Boards) -> IO ()
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
