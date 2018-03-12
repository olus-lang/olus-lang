module Lexer where

import Text.Parsec (SourceName, ParseError)
import Text.Parsec.Indent (IndentT, IndentParser, runIndentParser)
import Control.Monad.State (State)
import Data.Functor.Identity (Identity)
import qualified Text.Parsec as P
import qualified Text.Parsec.Token as Tok

type IParser a = IndentParser String () a

iParse :: IParser a -> SourceName -> String -> Either ParseError a
iParse aParser = runIndentParser aParser ()

languageDef :: Tok.GenLanguageDef String st (IndentT Identity)
languageDef = Tok.LanguageDef 
  { Tok.commentStart   = ""
  , Tok.commentEnd     = ""
  , Tok.commentLine    = ""
  , Tok.nestedComments = True
  , Tok.identStart     = P.letter P.<|> P.char '_'
  , Tok.identLetter    = P.alphaNum P.<|> P.oneOf "_'"
  , Tok.opStart        = Tok.opLetter languageDef
  , Tok.opLetter       = P.oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.reservedOpNames= []
  , Tok.reservedNames  = [":"]
  , Tok.caseSensitive  = True
  }

lexer :: Tok.GenTokenParser String () (IndentT Identity)
lexer = Tok.makeTokenParser languageDef

parens        = Tok.parens lexer
braces        = Tok.braces lexer
identifier    = Tok.identifier lexer
integer       = Tok.integer lexer
stringLiteral = Tok.stringLiteral lexer
reserved      = Tok.reserved lexer
