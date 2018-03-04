module Main where

import Parser
import Interpreter (Environment, execStatements)
import Control.Monad.Trans
import System.Console.Haskeline (getInputLine, runInputT, defaultSettings, outputStrLn)
import System.Environment (getArgs)
import Data.Map (empty)
import Control.Monad.State (execState)
import Builtins (initialEnvironment)

process :: Environment -> String -> IO (Maybe Environment)
process env source =
  case parseToplevel source of
    Left err -> do
      print $ "Error: " ++ show err
      return Nothing
    Right statements -> do
      print env
      mapM_ print statements
      case execStatements env statements of
        Left err -> do
          print $ "Error: " ++ show err
          return Nothing
        Right newEnv -> return $ Just newEnv

processFile :: String -> IO (Maybe Environment)
processFile fname = readFile fname >>= process initialEnvironment

repl :: IO ()
repl = runInputT defaultSettings (loop initialEnvironment)
  where
  loop env = do
    minput <- getInputLine "ready> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
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
    [fname] -> processFile fname >> return ()
