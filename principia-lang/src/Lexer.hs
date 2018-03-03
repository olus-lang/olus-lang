module Lexer where

import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Token as Tok

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    ops = []
    names = [":"]
    style = emptyDef {
               Tok.commentLine = "#"
             , Tok.reservedOpNames = ops
             , Tok.reservedNames = names
             }

identifier :: Parser String
identifier = Tok.identifier lexer

integer :: Parser Integer
integer = Tok.integer lexer

stringLiteral :: Parser String
stringLiteral = Tok.stringLiteral lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer
