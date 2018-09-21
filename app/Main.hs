module Main where

import System.Environment (getArgs)

import Olus.CompilerUtils (compile)
import Olus.Interpreter.Interpreter (run)

processFile :: FilePath -> IO ()
processFile fname = do
  prg <- compile fname
  run prg

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fname] -> processFile fname
    _       -> putStr "Incorrect arguments"
