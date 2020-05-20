module Main where

import Control.Monad (void)
import Data.Default.Class
import Data.Foldable (traverse_)
import Data.Proxy (Proxy)
import GHC.Generics
import Network.Wai.Handler.Warp (Port)
import Servant
import System.Console.GetOpt
import System.Environment (getArgs)

import qualified Go.Board.Default as D
import qualified Go.Game.End as G

import Server

data OptArgs = OptArgs { directory :: FilePath
                       , port      :: Port
                       }
  deriving (Eq, Generic, Ord, Read, Show)

instance Default OptArgs where
  def = OptArgs { directory = "result/public" -- TODO: use cabal for static path?
                , port = 8022
                }

options :: [OptDescr (OptArgs -> IO OptArgs)]
options = [ Option ['d'] ["directory"]
              (ReqArg
                (\ arg opt -> return opt { directory = arg })
                "path")
              "Specify directory with static files"
          ,  Option ['p'] ["port"]
              (ReqArg
                (\ arg opt -> return opt { port = read arg })
                "port")
              "Specify port to listen on"
          ]

-- TODO: use?: https://cabal.readthedocs.io/en/latest/cabal-package.html?highlight=getDataDir#accessing-data-files-from-package-code
main :: IO ()
main = do args <- getArgs
          let (optArgs , nonOptArgs , _) = getOpt Permute options args
          opts <- foldl (>>=) (return def) optArgs
          let OptArgs { directory = directory
                      , port = port
                      } = opts
          void (server port directory :: IO (G.EndScreen (D.Board 13 2) 2))
