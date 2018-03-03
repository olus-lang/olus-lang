module Syntax where

type Identifier = String

data Expr
  = Var Identifier
  | LitInt Integer
  | LitStr String
  deriving (Eq, Ord, Show)

data Statement
  = Call [Expr]
  | Closure [Identifier] [Expr]
  deriving (Eq, Ord, Show)
