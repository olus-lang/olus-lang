{-# LANGUAGE LambdaCase #-}

module Core where

import Data.Map.Strict hiding (map)

import Syntax
import Interpreter

data Program = Program {
    constants :: Map Identifier Value,
    procedures :: [([Identifier], [Identifier])]
  }

instance Monoid Program where
  mempty = Program { constants = fromList [], procedures = [] }
  mappend a b = Program {
    constants = constants b `union` constants a,
    procedures = procedures a ++ procedures b
  }

-- TODO Extract constants

extractExpr :: [Expr] -> [Identifier]
extractExpr = map $ \case Var x -> x

extract :: Scope -> Program
extract = \case
  Block statements           -> mconcat $ map extract statements
  Statement _                -> mempty
  Declaration name ids exprs -> Program {
    constants = fromList [],
    procedures = [(name:ids, extractExpr exprs)]
  }
