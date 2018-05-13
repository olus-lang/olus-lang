module CompilerUtils where

import System.FilePath (replaceExtension)
import System.Directory (doesFileExist, getModificationTime)
import Text.Megaparsec (parseErrorPretty')

import qualified Parser as P
import qualified Unparser as U
import qualified Syntax as S
import qualified Desugar as D
import Program (Program, canonicalize)
import qualified Compiler as C
import qualified Binary as B
import qualified ConstantExtraction as CE
import qualified DeadCodeElimination as DCE
import qualified Intrinsics as I
import qualified Binder as Bi

sourcePathToBin :: FilePath -> FilePath
sourcePathToBin = flip replaceExtension ".olus.bin"

reCompile :: FilePath -> FilePath -> IO Program
reCompile src bin = do
  putStrLn $ "🌀  Compiling " ++ src
  source <- readFile src
  case P.parseToplevel source of
    Left err -> do
      let msg = parseErrorPretty' source err
      putStrLn $ "\n❌  Error: \n" ++ msg
      error msg
    Right ast -> let
        prog = C.compile ast
        prog' = canonicalize $ DCE.removeDeadDeclarations prog
      in do
        print ast
        print prog
        print prog'
        B.encodeFile bin prog'
        return prog'

-- Cached compile
compile :: FilePath -> IO Program
compile source =
  let
    bin = sourcePathToBin source
  in do
    hasBin <- doesFileExist bin
    if hasBin then do
      sourceMod <- getModificationTime source
      binMod <- getModificationTime bin
      if sourceMod < binMod then do
        putStrLn $ "💾  Loading " ++ bin
        B.decodeFile bin
      else
        reCompile source bin
    else
      reCompile source bin
