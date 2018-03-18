module Desugar where

import Data.List (findIndex)
import Control.Monad.State

import Syntax

--
-- Monad for generating unique ids
--
type IdGen = State Int

runIdGen :: IdGen a -> a
runIdGen f = evalState f 0

genVariable :: IdGen Identifier
genVariable = do
  n <- get
  put (n + 1)
  return $ "id" ++ show n

visitCalls :: (Call -> IdGen Call) -> Scope -> IdGen Scope
visitCalls visitor s = case s of
  Block block -> do
    block' <- mapM (visitCalls visitor) block
    return $ Block block'
  Declaration name ids call -> do
    call' <- visitor call
    return $ Declaration name ids call'
  Statement call -> do
    call' <- visitor call
    return $ Statement call'

--
-- Glucase
--

mergeGlucose :: Call -> Call -> Maybe Call
mergeGlucose inner [] = return inner
mergeGlucose inner [x] = do
  x' <- mergeGlucose' inner x
  return [x']
mergeGlucose inner (x:xs) =
  case mergeGlucose' inner x of
    Just x' -> Just $ x':xs
    Nothing -> case mergeGlucose inner xs of
      Just xs' -> Just $ x:xs'
      Nothing -> Nothing

mergeGlucose' :: Call -> Expr -> Maybe Expr
mergeGlucose' inner (Fructose ids call) = do
  call' <- mergeGlucose inner call
  return $ Fructose ids call'
mergeGlucose' inner (Galactose call) = do
  call' <- mergeGlucose inner call
  return $ Galactose call'
mergeGlucose' inner _ = Nothing

mergeGlucose'' :: Scope -> Scope -> Maybe [Scope]
mergeGlucose'' (Declaration name ids outer) (Statement inner) = do
  call <- mergeGlucose inner outer
  return [Declaration name ids call]
mergeGlucose'' (Declaration name ids outer) (Block (Statement inner:bs)) = do
  call <- mergeGlucose inner outer
  return [Declaration name ids call, Block bs]
mergeGlucose'' _ _ = Nothing

glucase' :: [Scope] -> [Scope]
glucase' (a:b:xs) =
  case mergeGlucose'' a b of
    Just a' -> glucase' (a' ++ xs)
    Nothing -> a : glucase' (b:xs)
glucase' a = a

glucase :: Scope -> IdGen Scope
glucase (Block xs) = return $ Block $ glucase' xs
glucase a = return a

--
-- Fructase
--

isFructose :: Expr -> Bool
isFructose ex = case ex of
  (Fructose _ _) -> True
  _              -> False

findFirstFructose :: [Expr] -> Maybe ([Expr], Expr, [Expr])
findFirstFructose exs = case findIndex isFructose exs of
  Nothing -> Nothing
  Just n  -> Just (take n exs, exs !! n, drop (n + 1) exs)

fructaseCall :: Call -> IdGen (Call, [Scope])
fructaseCall exs = case findFirstFructose exs of
  Nothing -> return (exs, [])
  Just (before, Fructose ids call, after) -> do
    nid <- genVariable
    (outer, os) <- fructaseCall (before ++ [Var nid] ++ after)
    (inner, is) <- fructaseCall call
    return (outer, [Declaration nid ids inner] ++ is ++ os)

toBlock :: [Scope] -> [Scope]
toBlock [] = []
toBlock b@[Block _] = b
toBlock a = [Block a]

joinScopes :: [Scope] -> [Scope] -> [Scope]
joinScopes a [] = a
joinScopes [] b = b
joinScopes a b = case (last a, head b) of
  (Block a', Block b') -> init a ++ Block (a' ++ b') : tail b
  (_, _) -> a ++ b

concatScopes :: [[Scope]] -> [Scope]
concatScopes = foldr joinScopes []

fructase' :: Scope -> IdGen [Scope]
fructase' s = case s of
  Block block -> do
    block' <- mapM fructase' block
    return $ toBlock (concatScopes block')
  Declaration name ids call -> do
    (call', block) <- fructaseCall call
    return $ Declaration name ids call' : toBlock block
  Statement call -> do
    (call', block) <- fructaseCall call
    return $ Statement call' : toBlock block

fructase :: Scope -> IdGen Scope
fructase s = do
  s' <- fructase' s
  case s' of
    [Block b] -> return $ Block b
    [a] -> return a
    o -> return $ Block o

--
-- Galactase
--

isGalactose :: Expr -> Bool
isGalactose (Galactose _) = True
isGalactose _ = False

findFirstGalactose :: [Expr] -> Maybe ([Expr], Expr, [Expr])
findFirstGalactose exs = case findIndex isGalactose exs of
  Nothing -> Nothing
  Just n -> Just (take n exs, exs !! n, drop (n + 1) exs)

galactaseCall :: Call -> IdGen Call
galactaseCall exs = case findFirstGalactose exs of
  Nothing -> mapM f exs where
    f e = case e of
      Fructose ids call -> do
        call' <- galactaseCall call
        return $ Fructose ids call'
      e -> return e
  Just (before, Galactose call, after) -> do
    nid <- genVariable
    inner <- galactaseCall (before ++ [Var nid] ++ after)
    outer <- galactaseCall (call ++ [Fructose [nid] inner])
    return outer

galactase :: Scope -> IdGen Scope
galactase = visitCalls galactaseCall

--
-- Desugar
--

desugar :: Scope -> Scope
desugar s = runIdGen $ do
  s' <- glucase s
  s'' <- galactase s'
  s''' <- fructase s''
  return s'''
