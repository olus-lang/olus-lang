module Olus.Parser.Lexer where

import Control.Monad (void)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Olus.Parser.Base

lineComment :: Parser ()
lineComment = L.skipLineComment "#"

-- space consumer with newline
scn :: Parser ()
scn = L.space space1 lineComment empty

-- space consumer without newline
spaceConsumer :: Parser ()
spaceConsumer = L.space (void $ takeWhile1P Nothing f) lineComment empty
  where
    f x = x == ' ' || x == '\t'

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: String -> Parser ()
symbol str = void $ L.symbol spaceConsumer str

identifier :: Parser String
identifier = lexeme $ (:) <$> letterChar <*> many alphaNumChar

stringLiteral :: Parser String
stringLiteral = lexeme $ char '"' >> manyTill L.charLiteral (char '"')

integer :: Parser Integer
integer = lexeme L.decimal

parenOpen :: Parser ()
parenOpen = symbol "("

parenClose :: Parser ()
parenClose = symbol ")"

maplet :: Parser ()
maplet = symbol ":"
