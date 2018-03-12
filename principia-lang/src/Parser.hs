module Parser where

import Text.Parsec (ParseError, eof, try, many, many1, (<|>))

import Lexer
import Syntax

-- Derive a parser with support for identation

var :: IParser Expr
var = do
  s <- identifier
  return $ Var s

litInt :: IParser Expr
litInt = do
  n <- integer
  return $ LitInt n

litStr :: IParser Expr
litStr = do
  s <- stringLiteral
  return $ LitStr s

fructose :: IParser Expr
fructose = parens $ do
  parameters <- many identifier 
  reserved ":"
  call <- many expr
  return $ Fructose parameters call

galactose :: IParser Expr
galactose = parens $ do
  call <- many expr
  return $ Galactose call

expr :: IParser Expr
expr = var <|> litInt <|> litStr <|> try fructose <|> try galactose

call :: IParser Scope
call = do
  closure <- expr
  arguments <- many expr
  return $ Call closure arguments

declaration :: IParser Scope
declaration = do
  name <- identifier
  parameters <- many identifier
  reserved ":"
  call <- many expr
  return $  Declaration name parameters call

scope :: IParser Scope
scope = try declaration <|> try call

contents :: IParser a -> IParser a
contents p = do
  r <- p
  eof
  return r

parseExpr :: String -> Either ParseError Expr
parseExpr s = iParse (contents expr) "<stdin>" s

parseToplevel :: String -> Either ParseError Scope
parseToplevel s = iParse (contents scope) "<stdin>" s
