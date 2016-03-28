{-# LANGUAGE FlexibleContexts #-}
module Main where

import Parser.Lexer
import Parser.Syntax
import Parser.Types

import Control.Monad.Trans

import System.Console.Haskeline
import System.Environment
import System.Exit

import Text.Parsec
import Text.Parsec.String

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

fromFile ::  String -> IO Module
fromFile file = do
  result <- parseFromFile moduleSignature file
  case result of
    Left err   -> die $ show err
    Right exps -> return exps

main :: IO ()
main = do
  args <- getArgs
  if null args
     then
      interactive
      else do
        modules <- mapM fromFile args
        mapM_ print modules
