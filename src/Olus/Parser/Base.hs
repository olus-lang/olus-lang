module Olus.Parser.Base where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String
