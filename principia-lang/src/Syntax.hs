module Syntax where

data Binder = Binder String Int
  deriving (Eq, Ord, Show)

type Name = Binder

type Parameters = [Binder]

type Call = [Expression]

data Expression
  = Reference String Int
  | Intrinsic String Int
  | LiteralInteger Integer
  | LiteralString String
  | Fructose Parameters Call
  | Galactose Call
  deriving (Eq, Ord, Show)

data Scope
  = Block [Scope]
  | Declaration Name Parameters Call
  | Statement Call
  deriving (Eq, Ord, Show)

undefined :: Int
undefined = -1
