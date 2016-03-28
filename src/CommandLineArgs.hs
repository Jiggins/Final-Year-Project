module CommandLineArgs where

import System.Console.GetOpt
import System.Environment

data Flag = Help
          | Version
          | Input (Maybe String)

usage ::  IO ()
usage = do
  putStrLn "usage"
  putStrLn "vlad [[-h | -v] | [source_file]]"

