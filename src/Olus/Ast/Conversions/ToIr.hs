module Olus.Ast.Conversions.ToIr where

import Data.Maybe (fromJust)
import Data.List (elem, elemIndex, nub)

import qualified Olus.Ast.Syntax as S
import qualified Olus.Ast.Passes.Binder as B
import qualified Olus.Ast.Passes.ConstantExtraction as CE
import qualified Olus.Ast.Passes.Desugar as D
import qualified Olus.Ir.Program as P
import qualified Olus.Ir.Intrinsics as I

convertConstant :: (String -> Maybe Int) -> S.Expression -> P.Constant
convertConstant builtinLookup = \case
  S.LiteralInteger n  -> P.Integer n
  S.LiteralString s   -> P.String s
  S.Reference s _     -> case builtinLookup s of
    Just i   -> P.Intrinsic i
    Nothing  -> error "Builtin not found"
  _                   -> error "Expression is not a constant"

extractDeclarations :: S.Scope -> [([Int],[Int])]
extractDeclarations = fScope where
  fScope :: S.Scope -> [([Int],[Int])]
  fScope = \case
    S.Block a            -> concatMap fScope a
    S.Declaration a b c  -> [(map fBinder $ a:b, map fExpression c)]
    _                    -> []
  
  fExpression :: S.Expression -> Int
  fExpression = \case
    S.Reference _ n  -> n
    _                -> error "Expecting only References in declarations"
  
  fBinder :: S.Binder -> Int
  fBinder (S.Binder _ n) = n

extractStatements :: S.Scope -> [[Int]]
extractStatements = fScope where
  
  fScope :: S.Scope -> [[Int]]
  fScope = \case
    S.Block a      -> concatMap fScope a
    S.Statement a  -> [map fExpression a]
    _              -> []
  
  fExpression :: S.Expression -> Int
  fExpression = \case
    S.Reference _ n  -> n
    _                -> error "Expecting only References in declarations"

extractIdentifiers :: S.Scope -> [String]
extractIdentifiers = fScope where
  
  fScope :: S.Scope -> [String]
  fScope = \case
    S.Block a            -> concatMap fScope a
    S.Declaration a b c  -> map fBinder $ a:b
    _                    -> []
  
  fBinder :: S.Binder -> String
  fBinder (S.Binder s _) = s

toIr :: S.Scope -> P.Program
toIr scope = program where
  
  isBuiltin :: String -> Bool
  isBuiltin = flip elem I.intrinsics
  
  builtinLookup :: String -> Maybe Int
  builtinLookup = flip elemIndex I.intrinsics
  
  -- Bind references counting creating indices from zero.
  -- Flatten scopes to a single block.
  scope' = B.bindingPass 0 scope
  
  -- Replace syntax sugar by simple plain declarations
  scope'' = D.desugar scope'
  
  -- Turn constants into references. Indices start from zero.
  -- Existing indices are shifted up.
  (constants, scope''') = CE.extractConstants isBuiltin scope''
  
  constants' = map (convertConstant builtinLookup) constants
  
  -- Extract declarations and statements
  declarations = extractDeclarations scope'''
  statements = extractStatements scope'''
  identifiers = (map show constants) ++ extractIdentifiers scope'''
  
  program = P.empty {
    P.constants = constants',
    P.declarations = declarations,
    P.statements = statements,
    P.identifiers = identifiers
  }
