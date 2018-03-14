module Syntax where

type Identifier = String

type Call = [Expr]

data Expr
  = Var Identifier
  | LitInt Integer
  | LitStr String
  | Fructose [Identifier] Call
  | Galactose Call
  deriving (Eq, Ord, Show)

data Scope
  = Block [Scope]
  | Declaration Identifier [Identifier] Call
  | Statement Call
  deriving (Eq, Ord, Show)
