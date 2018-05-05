module Desugar where

import Data.List (findIndex)
import Control.Monad.State

import qualified Syntax as S

--
-- Monad for generating unique ids
--
type IdGen = State Int

runIdGen :: IdGen a -> a
runIdGen f = evalState f 0

genVariable :: IdGen (S.Binder, S.Expression)
genVariable = do
  n <- get
  put (n + 1)
  let name = "id" ++ show n in
    return (S.Binder name n, S.Reference name n)

visitCalls :: (S.Call -> IdGen S.Call) -> S.Scope -> IdGen S.Scope
visitCalls visitor s = case s of
  S.Block block -> do
    block' <- mapM (visitCalls visitor) block
    return $ S.Block block'
  S.Declaration name ids call -> do
    call' <- visitor call
    return $ S.Declaration name ids call'
  S.Statement call -> do
    call' <- visitor call
    return $ S.Statement call'

--
-- Glucase
--

mergeGlucose :: S.Call -> S.Call -> Maybe S.Call
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

mergeGlucose' :: S.Call -> S.Expression -> Maybe S.Expression
mergeGlucose' inner (S.Fructose ids call) = do
  call' <- mergeGlucose inner call
  return $ S.Fructose ids call'
mergeGlucose' inner (S.Galactose call) = do
  call' <- mergeGlucose inner call
  return $ S.Galactose call'
mergeGlucose' inner _ = Nothing

mergeGlucose'' :: S.Scope -> S.Scope -> Maybe [S.Scope]
mergeGlucose'' (S.Declaration name ids outer) (S.Statement inner) = do
  call <- mergeGlucose inner outer
  return [S.Declaration name ids call]
mergeGlucose'' (S.Declaration name ids outer) (S.Block (S.Statement inner:bs)) = do
  call <- mergeGlucose inner outer
  return [S.Declaration name ids call, S.Block bs]
mergeGlucose'' _ _ = Nothing

glucase' :: [S.Scope] -> [S.Scope]
glucase' (a:b:xs) =
  case mergeGlucose'' a b of
    Just a' -> glucase' (a' ++ xs)
    Nothing -> a : glucase' (b:xs)
glucase' a = a

glucase :: S.Scope -> IdGen S.Scope
glucase (S.Block xs) = return $ S.Block $ glucase' xs
glucase a = return a

--
-- Fructase
--

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

fructase :: S.Scope -> IdGen S.Scope
fructase s = do
  s' <- fructase' s
  case s' of
    [S.Block b] -> return $ S.Block b
    [a] -> return a
    o -> return $ S.Block o

--
-- Galactase
--

isGalactose :: S.Expression -> Bool
isGalactose (S.Galactose _) = True
isGalactose _ = False

findFirstGalactose :: [S.Expression] -> Maybe ([S.Expression], S.Expression, [S.Expression])
findFirstGalactose exs = case findIndex isGalactose exs of
  Nothing -> Nothing
  Just n -> Just (take n exs, exs !! n, drop (n + 1) exs)

galactaseCall :: S.Call -> IdGen S.Call
galactaseCall exs = case findFirstGalactose exs of
  Nothing -> mapM f exs where
    f e = case e of
      S.Fructose ids call -> do
        call' <- galactaseCall call
        return $ S.Fructose ids call'
      e -> return e
  Just (before, S.Galactose call, after) -> do
    (bind, ref) <- genVariable
    inner <- galactaseCall (before ++ [ref] ++ after)
    outer <- galactaseCall (call ++ [S.Fructose [bind] inner])
    return outer

galactase :: S.Scope -> IdGen S.Scope
galactase = visitCalls galactaseCall

--
-- Desugar
--

-- TODO: Make Fructase nested

-- TODO
--a:
--  b (:)
--    c

desugar :: S.Scope -> S.Scope
desugar s = runIdGen $ do
  s' <- glucase s
  s'' <- galactase s'
  s''' <- fructase s''
  return s'''
