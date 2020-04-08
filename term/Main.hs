{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving #-}

module Main where

import qualified Go.Board.Default as D
import qualified Go.Board.Loop as L
import Go.Config
import Go.Game.End
import Go.Run.Term

import System.Console.GetOpt
import System.Environment (getArgs)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Control.Monad (void)

data Board = Default
           | Loop
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

readBoard :: String -> Board
readBoard string = fromMaybe err $ readMaybe string
  where err = let boards = map show ([ minBound .. maxBound ] :: [Board])
               in error $ "Choose board from: " ++ intercalate ", " boards

newtype Options = Options { optBoard :: Board }
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

instance Default Options where
  def = Options { optBoard = Default }

options :: [OptDescr (Options -> IO Options)]
options = [ Option ['b'] ["board"]
              (ReqArg
                (\ arg opt -> return opt { optBoard = readBoard arg })
                "Board")
              "Board"
          ]

choose :: Board -> IO ()
choose board = case board of
                 Default -> void (game def :: IO (EndScreen (D.BoardSquare 2) 2))
                 Loop    -> void (game def :: IO (EndScreen (L.BoardLoop 2)   2))

main :: IO ()
main = do args <- getArgs
          let (optArgs , _ , _) = getOpt Permute options args
          opts <- foldl (>>=) (return def) optArgs
          let Options { optBoard = board
                      } = opts

          choose board