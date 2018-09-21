module Olus.Ast.Passes.Fructase where

import Data.List (findIndex)

import Olus.Ast.Utils.IdGen
import qualified Olus.Ast.Syntax as S

isFructose :: S.Expression -> Bool
isFructose ex = case ex of
  (S.Fructose _ _) -> True
  _              -> False

findFirstFructose :: [S.Expression] -> Maybe ([S.Expression], S.Expression, [S.Expression])
findFirstFructose exs = case findIndex isFructose exs of
  Nothing -> Nothing
  Just n  -> Just (take n exs, exs !! n, drop (n + 1) exs)

fructaseCall :: S.Call -> IdGen (S.Call, [S.Scope])
fructaseCall exs = case findFirstFructose exs of
  Nothing -> return (exs, [])
  Just (before, S.Fructose ids call, after) -> do
    (bind, ref) <- genVariable
    (outer, os) <- fructaseCall (before ++ [ref] ++ after)
    (inner, is) <- fructaseCall call
    return (outer, [S.Declaration bind ids inner] ++ is ++ os)

toBlock :: [S.Scope] -> [S.Scope]
toBlock [] = []
toBlock [S.Block a] = toBlock a
toBlock a = [S.Block a]

joinScopes :: [S.Scope] -> [S.Scope] -> [S.Scope]
joinScopes a [] = a
joinScopes [] b = b
joinScopes a b = case (last a, head b) of
  (S.Block a', S.Block b') -> init a ++ S.Block (a' ++ b') : tail b
  (_, _) -> a ++ b

concatScopes :: [[S.Scope]] -> [S.Scope]
concatScopes = foldr joinScopes []

fructase' :: S.Scope -> IdGen [S.Scope]
fructase' s = case s of
  S.Block block -> do
    block' <- mapM fructase' block
    return $ toBlock (concatScopes block')
  S.Declaration name ids call -> do
    (call', block) <- fructaseCall call
    return $ S.Declaration name ids call' : toBlock block
  S.Statement call -> do
    (call', block) <- fructaseCall call
    return $ S.Statement call' : toBlock block

--- TODO: Do not generate scope blocks
fructase :: S.Scope -> IdGen S.Scope
fructase s = do
  s' <- fructase' s
  case s' of
    [S.Block b] -> return $ S.Block b
    [a] -> return a
    o -> return $ S.Block o
