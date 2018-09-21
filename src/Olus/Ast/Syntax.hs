module Olus.Ast.Syntax where

data Binder = Binder String Int
  deriving (Eq, Ord, Show)

type Name = Binder

type Parameters = [Binder]

type Call = [Expression]

data Expression
  = Reference String Int
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

renumber :: (Int -> Int) -> Scope -> Scope
renumber fn = fScope where
  fScope :: Scope -> Scope
  fScope = \case
    Block a ->  Block $ map fScope a
    Declaration a b c -> Declaration (fBinder a) (map fBinder b) (map fExpression c)
    Statement a -> Statement $ map fExpression a
  fExpression :: Expression -> Expression
  fExpression = \case
    Fructose a b -> Fructose (map fBinder a) (map fExpression b)
    Galactose a -> Galactose (map fExpression a)
    Reference s n -> Reference s (fn n)
    a -> a
  fBinder :: Binder -> Binder
  fBinder (Binder s n) = Binder s (fn n)

-- Same as renumber expect undefined is preserved
renumber' :: (Int -> Int) -> Scope -> Scope
renumber' fn = renumber $ \case
  -1 -> -1
  n -> fn n
