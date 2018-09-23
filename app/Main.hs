module Main where

import Options.Applicative
import Data.Semigroup ((<>))

import Olus.CompilerUtils (compile)
import Olus.Interpreter.Interpreter (run)

processFile :: FilePath -> IO ()
processFile fname = do
  prg <- compile fname
  run prg

main :: IO ()
main = processFile =<< execParser opts
  where
    opts = info (arguments <**> helper)
      (  fullDesc
      <> header "olus - compiler and interpreter for the Oluś language"
      <> progDesc "Compile and run Oluś source files"
      )
    arguments = strOption
      (  long "input"
      <> short 'i'
      <> metavar "INPUT"
      <> help "Oluś source file"
      )
