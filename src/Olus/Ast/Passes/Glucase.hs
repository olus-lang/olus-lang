module Olus.Ast.Passes.Glucase where

import Olus.Ast.Utils.IdGen
import qualified Olus.Ast.Syntax as S

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
