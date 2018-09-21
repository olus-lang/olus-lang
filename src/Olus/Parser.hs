module Olus.Parser where

import Data.Void
import Text.Megaparsec (ParseError, Token, parse)

import Olus.Ast.Syntax (Expression, Scope)
import Olus.Parser.Parser

parseExpr :: String -> Either (ParseError (Token String) Void) Expression
parseExpr s = parse (contents expr) "" s

parseToplevel :: String -> Either (ParseError (Token String) Void) Scope
parseToplevel s = parse (contents blocks) "" s

parseFile :: FilePath -> IO (Either (ParseError (Token String) Void) Scope)
parseFile f = do
  s <- readFile f
  return $ parse (contents blocks) f s
