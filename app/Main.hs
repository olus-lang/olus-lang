module Main where

import Control.Monad (void)
import Control.Monad.Trans
import System.Console.Haskeline (getInputLine, runInputT, defaultSettings, outputStrLn)
import System.Environment (getArgs)
import Text.Megaparsec (parseErrorPretty')

import Olus.Ast.Compiler
import Olus.Parser
import Olus.Ast.Unparser
import Olus.Ast.Passes.Desugar
import Olus.Ir.Binary
import qualified Olus.CompilerUtils as CU
import qualified Olus.Interpreter.Interpreter as I

type Environment = ()

initialEnvironment :: Environment
initialEnvironment = ()

process :: Environment -> String -> IO (Maybe Environment)
process env source =
  case parseToplevel source of
    Left err -> do
      putStrLn $ "\n❌  Error: \n" ++ parseErrorPretty' source err
      return Nothing
    Right scope -> do
      putStr $ unparse $ desugar scope
      print $ compile $ desugar scope
      print $ toHex $ encode $ compile $ desugar scope
      encodeFile "test.bin" $ compile $ desugar scope
      return Nothing

processFile :: FilePath -> IO ()
processFile fname = do
  prg <- CU.compile fname
  print prg
  I.run prg

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
          Just newEnv' -> loop newEnv'
          Nothing -> loop env

main :: IO ()
main = do
  args <- getArgs
  case args of
    []      -> repl
    [fname] -> processFile fname
    _       -> print "To many arguments"
