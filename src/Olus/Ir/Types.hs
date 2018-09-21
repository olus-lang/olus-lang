module Olus.Ir.Types where

import Olus.Ast.Syntax

-- http://dev.stephendiehl.com/fun/006_hindley_milner.html

data Type
  = Integer
  | String
  | Closure [Type]
  | Typeof Int
  deriving (Show, Eq, Ord)

-- constEqs :: [P.Constant] -> [(Int, Type)]

declEqs :: ([Int],[Int]) -> [(Int, Type)]
declEqs (name:parameters, closure:arguments) = [
  (name, Closure $ map Typeof parameters),
  (closure, Closure $ map Typeof arguments)]
