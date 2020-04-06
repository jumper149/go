{-# LANGUAGE DataKinds #-}

import qualified Go.Board.Default as D
import qualified Go.Board.Loop as L
import Go.Config
import Go.Game.End
import Go.Run.Client.JSONTerm
import Go.Run.Server.JSON
import Go.Run.Term

import System.Console.GetOpt
import System.Environment (getArgs)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Control.Monad (void)

data Run = Local LocalInterface
         | Server ServerAPI
         | Client ClientAPI ClientInterface
  deriving (Eq, Read, Show)

instance Default Run where
  def = Local LocalTerm

data LocalInterface = LocalTerm
  deriving (Eq, Read, Show, Enum, Bounded)

instance Default LocalInterface where
  def = LocalTerm

data ServerAPI = ServerJSON
  deriving (Eq, Read, Show, Enum, Bounded)

instance Default ServerAPI where
  def = ServerJSON

data ClientAPI = ClientJSON
  deriving (Eq, Read, Show, Enum, Bounded)

instance Default ClientAPI where
  def = ClientJSON

data ClientInterface = ClientTerm
  deriving (Eq, Read, Show, Enum, Bounded)

instance Default ClientInterface where
  def = ClientTerm

readRunningMode :: String -> Run
readRunningMode string = fromMaybe (error "RunningMode can't be parsed") $ readMaybe string

data Board = Default
           | Loop
  deriving (Eq, Read, Show, Enum, Bounded)

errBoard :: Board
errBoard = error $ "Choose board from: " ++ intercalate ", " boards
  where boards = map show ([ minBound .. maxBound ] :: [Board])

readBoard :: String -> Board
readBoard string = fromMaybe errBoard $ readMaybe string

data Options = Options { optRunningMode :: Run
                       , optBoard       :: Board
                       }

instance Default Options where
  def = Options { optRunningMode = Local LocalTerm
                , optBoard = Default
                }

options :: [OptDescr (Options -> IO Options)]
options = [ Option ['r'] ["run"]
              (ReqArg
                (\ arg opt -> return opt { optRunningMode = readRunningMode arg })
                "RunningMode")
              "RunningMode"
          , Option ['b'] ["board"]
              (ReqArg
                (\ arg opt -> return opt { optBoard = readBoard arg })
                "Board")
              "Board"
          ]

choose :: Run -> Board -> IO ()
choose (Local LocalTerm) board = case board of
                                   Default -> void (game def :: IO (EndScreen (D.BoardSquare 2) 2))
                                   Loop    -> void (game def :: IO (EndScreen (L.BoardLoop 2)   2))
choose (Server ServerJSON) board = case board of
                                     Default -> void (serverJSON def :: IO (EndScreen (D.BoardSquare 2) 2))
                                     Loop    -> void (serverJSON def :: IO (EndScreen (L.BoardLoop 2)   2))
choose (Client ClientJSON ClientTerm) board = case board of
                                                Default -> void (clientJSONTerm :: IO (EndScreen (D.BoardSquare 2) 2))
                                                Loop    -> void (clientJSONTerm :: IO (EndScreen (L.BoardLoop 2)   2))
--choose _ _ = error "This combination of running-mode and board is not supported."

main :: IO ()
main = do args <- getArgs
          let (optArgs , _ , _) = getOpt Permute options args
          opts <- foldl (>>=) (return def) optArgs
          let Options { optRunningMode = runningMode
                      , optBoard = board
                      } = opts

          choose runningMode board
