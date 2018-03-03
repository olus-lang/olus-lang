module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Lexer
import Syntax

table = [[]
        ,[]]

call :: Parser Call
call = do
  func <- expr
  args <- many expr
  return $ Call func args

closure :: Parser Closure
closure = do
  func <- identifier
  args <- many identifier
  reserved ":"
  body <- call
  return $  Closure func args body

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

closureStatement :: Parser Statement
closureStatement = do
  closure <- closure
  return $ ClosureStatement closure

callStatement :: Parser Statement
callStatement = do
  call <- call
  return $ CallStatement call

statement :: Parser Statement
statement = try closureStatement <|> callStatement

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
