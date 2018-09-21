module Olus.Ast.Passes.Desugar where

import Data.List (findIndex)

import Olus.Ast.Utils.IdGen
import qualified Olus.Ast.Syntax as S
import qualified Olus.Ast.Passes.Binder as B
import Olus.Ast.Passes.Glucase (glucase)
import Olus.Ast.Passes.Fructase (fructase)
import Olus.Ast.Passes.Galactase (galactase)

-- Combined pass that desugars

desugar :: S.Scope -> S.Scope
desugar s = runIdGen s $ do
  s' <- glucase s
  s'' <- galactase s'
  s''' <- fructase s''
  return $ B.flattenScopes s'''
