module Main where

import Frontend.Term.Term
import qualified Board.Default as D
import qualified Board.Loop as L

import System.Console.GetOpt
import System.Environment ( getArgs
                          )
import Data.List ( intercalate
                 )
import Text.Read ( readMaybe
                 )
import Control.Monad ( void
                     )

data Interface = Term
  deriving (Read, Show, Enum)

errInterface :: Interface
errInterface = error $ "choose interface from: " ++ intercalate ", " interfaces
  where interfaces = map show ([ toEnum 0 .. ] :: [Interface])

readInterface :: String -> Interface
readInterface string = maybe errInterface id $ readMaybe string

data Board = Default
           | Loop
  deriving (Read, Show, Enum)

errBoard :: Board
errBoard = error $ "choose board from: " ++ intercalate ", " boards
  where boards = map show ([ toEnum 0 .. ] :: [Board])

readBoard :: String -> Board
readBoard string = maybe errBoard id $ readMaybe string

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
                (\ arg opt -> return opt { optInterface = readInterface arg })
                "Interface")
              "Interface"
          , Option ['b'] ["board"]
              (ReqArg
                (\ arg opt -> return opt { optBoard = readBoard arg })
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
