{-# LANGUAGE TupleSections #-}

module Parser where

import Control.Monad (void)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Syntax

type Parser = Parsec Void String

--
-- Lexer
--

lineComment :: Parser ()
lineComment = L.skipLineComment "#"

-- space consumer with newline
scn :: Parser ()
scn = L.space space1 lineComment empty

-- space consumer without newline
sc :: Parser ()
sc = L.space (void $ takeWhile1P Nothing f) lineComment empty
  where
    f x = x == ' ' || x == '\t'

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

identifier :: Parser String
identifier = lexeme $ (:) <$> letterChar <*> many alphaNumChar

stringLiteral :: Parser String
stringLiteral = char '"' >> manyTill L.charLiteral (char '"')

integer :: Parser Integer
integer = lexeme L.decimal

--
-- Parser
--

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
-- TODO Suppress newline and indentation in parens?

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

fructose :: Parser Expr
fructose = parens $ do
  parameters <- many identifier 
  symbol ":"
  call <- many expr
  return $ Fructose parameters call

galactose :: Parser Expr
galactose = parens $ do
  call <- many expr
  return $ Galactose call

expr :: Parser Expr
expr = var <|> litInt <|> litStr <|> try fructose <|> try galactose

call :: Parser Scope
call = do
  closure <- expr
  arguments <- many expr
  return $ Call closure arguments

declaration :: Parser Scope
declaration = do
  name <- identifier
  parameters <- many identifier
  symbol ":"
  call <- many expr
  return $ Declaration name parameters call

statement :: Parser Scope
statement = try declaration <|> try call

block :: Parser [Scope]
block = L.indentBlock scn $ do
  header <- statement
  return $ L.IndentMany Nothing (return . f header) block
  where
    f :: Scope -> [[Scope]] -> [Scope]
    f h [] = [h]
    f h x  = [h, Block $ concat x]

blocks :: Parser Scope
blocks = do
  blks <- many block
  return $ Block $ concat blks

scope :: Parser Scope
scope = try declaration <|> try call

contents :: Parser a -> Parser a
contents p = between scn eof p

parseExpr :: String -> Either (ParseError (Token String) Void) Expr
parseExpr s = parse (contents expr) "<stdin>" s

parseToplevel :: String -> Either (ParseError (Token String) Void) Scope
parseToplevel s = parse (contents blocks) "<stdin>" s
