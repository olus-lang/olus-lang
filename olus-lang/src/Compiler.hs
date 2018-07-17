{-# LANGUAGE LambdaCase #-}
module  Compiler where

import Data.Maybe (fromJust)
import Data.List (elemIndex, nub)
import qualified Syntax as S
import qualified Program as P
import qualified Binder as B
import qualified Desugar as D
import qualified ConstantExtraction as CE
import qualified DeadCodeElimination as DCE

extractDeclarations :: S.Scope -> [([Int],[Int])]
extractDeclarations = fScope where
  fScope :: S.Scope -> [([Int],[Int])]
  fScope = \case
    S.Block a -> concatMap fScope a
    S.Declaration a b c -> [(map fBinder $ a:b, map fExpression c)]
    _ -> []
  fExpression :: S.Expression -> Int
  fExpression = \case
    S.Reference _ n -> n
    _ -> error "Expecting only References in declarations"
  fBinder :: S.Binder -> Int
  fBinder (S.Binder _ n) = n

extractStatements :: S.Scope -> [[Int]]
extractStatements = fScope where
  fScope :: S.Scope -> [[Int]]
  fScope = \case
    S.Block a -> concatMap fScope a
    S.Statement a -> [map fExpression a]
    _ -> []
  fExpression :: S.Expression -> Int
  fExpression = \case
    S.Reference _ n -> n
    _ -> error "Expecting only References in declarations"

extractIdentifiers :: S.Scope -> [String]
extractIdentifiers = fScope where
  fScope :: S.Scope -> [String]
  fScope = \case
    S.Block a -> concatMap fScope a
    S.Declaration a b c -> map fBinder $ a:b
    _ -> []
  fBinder :: S.Binder -> String
  fBinder (S.Binder s _) = s

compile :: S.Scope -> P.Program
compile scope = program where
  
  -- Bind references counting creating indices from zero.
  -- Flatten scopes to a single block.
  scope' = B.bindingPass 0 scope
  
  -- Replace syntax sugar by simple plain declarations
  scope'' = D.desugar scope'
  
  -- Turn constants into references. Indices start from zero.
  -- Existing indices are shifted up.
  (constants, scope''') = CE.extractConstants scope''
  
  -- Extract declarations and statements
  declarations = extractDeclarations scope'''
  statements = extractStatements scope'''
  identifiers = (map show constants) ++ extractIdentifiers scope'''
  
  program = P.empty {
    P.constants = constants,
    P.declarations = declarations,
    P.statements = statements,
    P.identifiers = identifiers
  }
