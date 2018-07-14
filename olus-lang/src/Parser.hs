{-# LANGUAGE TupleSections #-}

module Parser where

import Control.Monad (void)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Syntax as S

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

identifier' :: Parser String
identifier' = lexeme $ (:) <$> letterChar <*> many alphaNumChar

stringLiteral :: Parser String
stringLiteral = lexeme $ char '"' >> manyTill L.charLiteral (char '"')

integer :: Parser Integer
integer = lexeme L.decimal

--
-- Parser
--

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
-- TODO Suppress newline and indentation in parens?

binder :: Parser S.Binder
binder = do
  s <- identifier'
  return $ S.Binder s S.undefined

reference :: Parser S.Expression
reference = do
  s <- identifier'
  return $ S.Reference s S.undefined

litInt :: Parser S.Expression
litInt = do
  n <- integer
  return $ S.LiteralInteger n

litStr :: Parser S.Expression
litStr = do
  s <- stringLiteral
  return $ S.LiteralString s

fructose :: Parser S.Expression
fructose = parens $ do
  parameters <- many binder 
  symbol ":"
  call <- many expr
  return $ S.Fructose parameters call

galactose :: Parser S.Expression
galactose = parens $ do
  call <- many expr
  return $ S.Galactose call

expr :: Parser S.Expression
expr = reference <|> litInt <|> litStr <|> try fructose <|> try galactose

call :: Parser S.Call
call = do
  closure <- expr
  arguments <- many expr
  return $ closure : arguments

declaration :: Parser S.Scope
declaration = do
  name <- binder
  parameters <- many binder
  symbol ":"
  call <- many expr
  return $ S.Declaration name parameters call

statement :: Parser S.Scope
statement = do 
  c <- call
  return $ S.Statement c

block :: Parser [S.Scope]
block = L.indentBlock scn $ do
  header <- try declaration <|> statement
  return $ L.IndentMany Nothing (return . f header) block
  where
    f :: S.Scope -> [[S.Scope]] -> [S.Scope]
    f h [] = [h]
    f h x  = [h, S.Block $ concat x]

blocks :: Parser S.Scope
blocks = do
  blocks <- many block
  return $ S.Block $ concat blocks

contents :: Parser a -> Parser a
contents p = between scn eof p

parseExpr :: String -> Either (ParseError (Token String) Void) S.Expression
parseExpr s = parse (contents expr) "<stdin>" s

parseToplevel :: String -> Either (ParseError (Token String) Void) S.Scope
parseToplevel s = parse (contents blocks) "<stdin>" s
