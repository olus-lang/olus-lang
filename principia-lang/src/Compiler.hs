{-# LANGUAGE LambdaCase #-}
module  Compiler where

import Data.Maybe (fromJust)
import Data.List (elemIndex, nub)
import qualified Syntax as S
import qualified Program as P

listConstants :: S.Scope -> [P.Constant]
listConstants = nub . \case
  S.Block b           -> concatMap listConstants b
  S.Declaration _ _ a -> concatMap listConstants' a
  S.Statement _       -> []

listConstants' :: S.Expression -> [P.Constant]
listConstants' = \case
  S.LiteralInteger n -> [P.Integer n]
  S.LiteralString s -> [P.String s]
  _          -> []

listIdsAndConst :: S.Scope -> P.Program
listIdsAndConst s = P.empty {
  -- TODO P.identifiers  = listIdentifiers s,
  P.constants    = listConstants s
}

idToIndex :: P.Program -> S.Binder -> Int
idToIndex p (S.Binder s n) = n

idsToIndices :: P.Program -> [S.Binder] -> [Int]
idsToIndices p = map (idToIndex p)

exToIndex :: P.Program -> S.Expression -> Int
exToIndex p = \case
  S.LiteralInteger n -> li + fromJust (elemIndex (P.Integer n) c)
  S.LiteralString s -> li + fromJust (elemIndex (P.String s) c)
  S.Reference _ n    -> n
  where
    li = length (P.identifiers p)
    c = P.constants p

exsToIndices :: P.Program -> [S.Expression] -> [Int]
exsToIndices p = map (exToIndex p)

toDeclaration :: P.Program -> [S.Binder] -> [S.Expression] -> ([Int], [Int])
toDeclaration p a b = (idsToIndices p a, exsToIndices p b)

listDeclarations :: P.Program -> S.Scope -> [([Int], [Int])]
listDeclarations p = \case
  S.Block b           -> concatMap (listDeclarations p) b
  S.Declaration a b c -> [toDeclaration p (a : b) c]
  S.Statement _       -> []
  
listCalls :: P.Program -> S.Scope -> [[Int]]
listCalls p = \case
  S.Block b       -> concatMap (listCalls p) b
  S.Declaration{} -> []
  S.Statement e   -> [exsToIndices p e]


addIndex :: P.Program -> P.Program
addIndex = id -- TODO

compile :: S.Scope -> P.Program
compile s = addIndex $ p {
  P.declarations = listDeclarations p s,
  P.calls        = listCalls p s
} where p = listIdsAndConst s
