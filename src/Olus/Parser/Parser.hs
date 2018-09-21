module Olus.Parser.Parser where

import Data.Void
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L

import Olus.Parser.Base
import Olus.Parser.Lexer
import qualified Olus.Ast.Syntax as S

parens :: Parser a -> Parser a
parens = between parenOpen parenClose

binder :: Parser S.Binder
binder = do
  s <- identifier
  return $ S.Binder s S.undefined

reference :: Parser S.Expression
reference = do
  s <- identifier
  return $ S.Reference s S.undefined

litInt :: Parser S.Expression
litInt = do
  n <- integer
  return $ S.LiteralInteger n

litStr :: Parser S.Expression
litStr = do
  s <- stringLiteral
  return $ S.LiteralString s

expr :: Parser S.Expression
expr = reference <|> litInt <|> litStr <|> try fructose <|> try galactose
  
call :: Parser S.Call
call = do
  closure <- expr
  arguments <- many expr
  return $ closure : arguments

fructose :: Parser S.Expression
fructose = parens $ do
  parameters <- many binder 
  maplet
  call' <- call
  return $ S.Fructose parameters call'

galactose :: Parser S.Expression
galactose = parens $ do
  call' <- call
  return $ S.Galactose call'

declaration :: Parser S.Scope
declaration = do
  name <- binder
  parameters <- many binder
  maplet
  call' <- call
  return $ S.Declaration name parameters call'

statement :: Parser S.Scope
statement = do 
  call' <- call
  return $ S.Statement call'

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
  blocks' <- many block
  return $ S.Block $ concat blocks'

contents :: Parser a -> Parser a
contents p = between scn eof p
