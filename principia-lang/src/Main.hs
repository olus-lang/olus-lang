module Main where

import Control.Monad.Trans
import System.Console.Haskeline (getInputLine, runInputT, defaultSettings, outputStrLn)
import System.Environment (getArgs)
import Data.Map (empty)
import Control.Monad.State (execState)
import Text.Megaparsec (parseErrorPretty')

import Parser
import Unparser
import Desugar

data Environment = Environment deriving (Show)
initialEnvironment = Environment

process :: Environment -> String -> IO (Maybe Environment)
process env source =
  case parseToplevel source of
    Left err -> do
      putStrLn $ "\nError: \n" ++ parseErrorPretty' source err
      return Nothing
    Right scope -> do
      print env
      print scope
      putStr $ unparse $ desugar scope
      return $ Just env
      --case execStatements env scope of
      --  Left err -> do
      --    print $ "Error: " ++ show err
      --    return Nothing
      --  Right newEnv -> return $ Just newEnv

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
