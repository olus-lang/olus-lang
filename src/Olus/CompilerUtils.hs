module Olus.CompilerUtils where

import System.FilePath (replaceExtension)
import System.Directory (doesFileExist, getModificationTime)
import Text.Megaparsec (parseErrorPretty')

import qualified Olus.Parser as P
import Olus.Ast.Conversions
import qualified Olus.Ast.Syntax as S
import qualified Olus.Ast.Passes.Desugar as D
import Olus.Ir.Program (Program, canonicalize)
import qualified Olus.Ir.Binary as B
import qualified Olus.Ast.Passes.ConstantExtraction as CE
import qualified Olus.Ir.Passes.DeadCodeElimination as DCE
import qualified Olus.Ir.Intrinsics as I
import qualified Olus.Ast.Passes.Binder as Bi
import qualified Olus.Ir.Passes.Closure as Cl

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
        ast' = D.desugar $ Bi.bindingPass 0 ast
        modu = astToModule ast'
        prog = astToIr ast
        prog' = Cl.computeClosures $ canonicalize $ DCE.removeDeadDeclarations prog
      in do
        putStrLn "AST:"
        print ast
        putStrLn "AST':"
        print ast'
        putStrLn "Module:"
        print modu
        putStrLn "Compiled:"
        print prog
        putStrLn "Processed:"
        print prog'
        putStrLn "Closures:"
        putStrLn $ Cl.showClosures prog'
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
        prog <- B.decodeFile bin
        return $ Cl.computeClosures prog
      else
        reCompile source bin
    else
      reCompile source bin
