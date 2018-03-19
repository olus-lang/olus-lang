module Main where

import Control.Monad (void)
import Control.Monad.Trans
import System.Console.Haskeline (getInputLine, runInputT, defaultSettings, outputStrLn)
import System.Environment (getArgs)
import Data.Map.Strict (empty, fromList)
import Control.Monad.State (execState)
import Text.Megaparsec (parseErrorPretty')

import Syntax
import Parser
import Unparser
import Desugar
import Interpreter
import Builtins

process :: Environment -> String -> IO (Maybe Environment)
process env source =
  case parseToplevel source of
    Left err -> do
      putStrLn $ "\n❌  Error: \n" ++ parseErrorPretty' source err
      return Nothing
    Right scope -> do
      putStr $ unparse $ desugar scope
      result <- runInterpreter (desugar scope) env
      case result of
        Left err -> do
          putStrLn $ "\n❌  Error: " ++ err
          return Nothing
        Right newEnv -> return $ Just newEnv

processFile :: String -> IO (Maybe Environment)
processFile fname = readFile fname >>= process initialEnvironment

repl :: IO ()
repl = runInputT defaultSettings (loop initialEnvironment)
  where
  loop env = do
    minput <- getInputLine "\n✳️  "
    case minput of
      Nothing -> outputStrLn "👋  Goodbye!"
      Just input -> do
        newEnv <- liftIO $ process env input
        case newEnv of
          Just newEnv -> loop newEnv
          Nothing -> loop env

main :: IO ()
main = do
  args <- getArgs
  case args of
    []      -> repl
    [fname] -> void $ processFile fname
