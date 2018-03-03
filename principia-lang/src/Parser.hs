module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Lexer
import Syntax

table = [[]
        ,[]]

var :: Parser Expr
var = do
  s <- identifier
  return $ Var s

litInt :: Parser Expr
litInt = do
  n <- integer
  return $ LitInt n

litStr :: Parser Expr
litStr = do
  s <- stringLiteral
  return $ LitStr s

expr :: Parser Expr
expr = var <|> litInt <|> litStr

call :: Parser Statement
call = do
  callargs <- many1 expr
  return $ Call callargs

closure :: Parser Statement
closure = do
  funcargs <- many1 identifier
  reserved ":"
  callargs <- many1 expr
  return $  Closure funcargs callargs

statement :: Parser Statement
statement = try closure <|> call

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

toplevel :: Parser [Statement]
toplevel = many $ do
    def <- statement
    -- newline
    return def

parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (contents expr) "<stdin>" s

parseToplevel :: String -> Either ParseError [Statement]
parseToplevel s = parse (contents toplevel) "<stdin>" s
