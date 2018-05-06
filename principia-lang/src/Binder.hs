{-# LANGUAGE LambdaCase #-}
module Binder where

import Prelude hiding (lookup)
import Control.Monad.State
import Data.Map.Strict hiding (map, foldr)
import qualified Syntax as S

empty :: Map String Int
empty = Data.Map.Strict.empty

intrinsics :: [String]
intrinsics = ["isZero","add","print","mul","sub","exit","input","parseInt"]

maxBinderNumber :: S.Scope -> Int
maxBinderNumber = fScope where
  fScope :: S.Scope -> Int
  fScope = \case
    S.Block b -> foldr (max . fScope) 0 b
    S.Declaration a b c -> foldr max 0 $ fBinder a : map fBinder b ++ map fExpression c
    S.Statement a -> foldr (max . fExpression) 0 a
  fExpression :: S.Expression -> Int
  fExpression = \case
    S.Reference _ n -> n
    S.Fructose a b -> foldr max 0 $ map fBinder a ++ map fExpression b
    S.Galactose a -> foldr (max . fExpression) 0 a
    _ -> 0
  fBinder :: S.Binder -> Int
  fBinder (S.Binder _ n) = n

-- TODO: Return map from old numbers to new numbers
numberBinders :: Int -> S.Scope -> S.Scope
numberBinders start scope = evalState (numberScope scope) start where
  numberScope :: S.Scope -> State Int S.Scope
  numberScope = \case
    S.Block b -> do
      b' <- mapM numberScope b
      return $ S.Block b'
    S.Declaration a b c -> do
      a' <- numberBinder a
      b' <- mapM numberBinder b
      c' <- mapM numberExpression c
      return $ S.Declaration a' b' c'
    S.Statement a -> do
      a' <- mapM numberExpression a
      return $ S.Statement a'
  numberExpression :: S.Expression -> State Int S.Expression
  numberExpression = \case
    S.Fructose a b -> do
      a' <- mapM numberBinder a
      b' <- mapM numberExpression b
      return $ S.Fructose a' b'
    S.Galactose a -> do
      a' <- mapM numberExpression a
      return $ S.Galactose a'
    a -> return a
  numberBinder :: S.Binder -> State Int S.Binder
  numberBinder (S.Binder s _) = do
    n <- get
    put (n + 1)
    return $ S.Binder s n

-- TODO: Forward looking
bindReferences :: Map String Int -> S.Scope -> S.Scope
bindReferences initial scope = evalState (bindScope scope) initial where
  bindScope :: S.Scope -> State (Map String Int) S.Scope
  bindScope = \case
    S.Block b -> do
      env <- get
      b' <- mapM bindScope b
      put env
      return $ S.Block b'
    S.Declaration a b c -> do
      a' <- bindBinder a
      b' <- mapM bindBinder b
      c' <- mapM bindExpression c
      return $ S.Declaration a' b' c'
    S.Statement a -> do
      a' <- mapM bindExpression a
      return $ S.Statement a'
  bindExpression :: S.Expression -> State (Map String Int) S.Expression
  bindExpression = \case
    S.Fructose a b -> do
      a' <- mapM bindBinder a
      b' <- mapM bindExpression b
      return $ S.Fructose a' b'
    S.Galactose a -> do
      a' <- mapM bindExpression a
      return $ S.Galactose a'
    S.Reference s n -> do
      env <- get
      case lookup s env of
        Just n' -> return $ S.Reference s n'
        Nothing -> return $ S.Reference s n
    a -> return a
  bindBinder :: S.Binder -> State (Map String Int) S.Binder
  bindBinder (S.Binder s n) = do
    env <- get
    put $ insert s n env
    return $ S.Binder s n

flattenScopes :: S.Scope -> S.Scope
flattenScopes s = S.Block $ f s where
  f = \case
    S.Block a -> concatMap f a
    a -> [a]

bindingPass :: [String] -> S.Scope -> S.Scope
bindingPass intrinsics scope =
  flattenScopes $
  bindReferences (fromList $ zip intrinsics [0..]) $
  numberBinders (length intrinsics) scope 
