module Syntax where

type Identifier = String

data Expr
  = Var Identifier
  | LitInt Integer
  | LitStr String
  | Fructose [Identifier] [Expr]
  | Galactose [Expr]
  deriving (Eq, Ord, Show)

data Scope
  = Block [Scope]
  | Declaration Identifier [Identifier] [Expr]
  | Call Expr [Expr]
  deriving (Eq, Ord, Show)
