{-# LANGUAGE FlexibleContexts #-}
module Main where

import Parser.Lexer
import Parser.Syntax
import Parser.Types

import CommandLineArgs

import Control.Monad.Trans
import Data.Functor.Identity
import System.Console.GetOpt
import System.Console.Haskeline
import System.Environment
import System.Exit
import Text.Parsec hiding (runParser)

interactive :: IO ()
interactive = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "vlad> "
      case minput of
        Nothing -> outputStrLn "Goodbye."
        Just input -> interactiveLine input >> loop

interactiveLine :: MonadIO m => String -> InputT m ()
interactiveLine input = case parse expr "<interactive>" input of
                          Left p -> (outputStrLn . show) p
                          Right err -> (outputStrLn . show) err

fromFile :: FilePath -> IO ()
fromFile file = undefined

main :: IO ()
main = do
  args <- getArgs
  if null args
     then
      interactive
     else
      mapM_ print args

