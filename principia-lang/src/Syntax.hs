module Syntax where

newtype Identifier = Identifier String
  deriving (Eq, Ord)

instance Show Identifier where
  show (Identifier s) = s

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
