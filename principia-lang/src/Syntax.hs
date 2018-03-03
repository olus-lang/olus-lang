module Syntax where

type Identifier
  = String

data Expr
  = Var Identifier
  | LitInt Integer
  | LitStr String
  deriving (Eq, Ord, Show)

data Call
  = Call Expr [Expr]
  deriving (Eq, Ord, Show)

data Closure
  = Closure Identifier [Identifier] Call
  deriving (Eq, Ord, Show)

data Statement
  = CallStatement Call
  | ClosureStatement Closure
  deriving (Eq, Ord, Show)
