module Olus.Ast.Passes.Binder where

import Prelude hiding (lookup)
import Control.Monad.State
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import qualified Olus.Ast.Syntax as S

empty :: Map String Int
empty = Map.empty

maxBinderNumber :: S.Scope -> Int
maxBinderNumber = fScope where
  maxMap :: (a -> Int) -> [a] -> Int
  maxMap f = foldr (max . f) 0
  fScope :: S.Scope -> Int
  fScope = \case
    S.Block b -> maxMap fScope b
    S.Declaration a b c -> max (maxMap fBinder (a : b)) (maxMap fExpression c)
    S.Statement a -> maxMap fExpression a
  fExpression :: S.Expression -> Int
  fExpression = \case
    S.Reference _ n -> n
    S.Fructose a b -> max (maxMap fBinder a) (maxMap fExpression b)
    S.Galactose a -> maxMap fExpression a
    _ -> 0
  fBinder :: S.Binder -> Int
  fBinder (S.Binder _ n) = n

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

forwardReferences :: [S.Scope] -> Map String Int
forwardReferences scope = execState (forwardScope scope) Map.empty where
  forwardScope :: [S.Scope] -> State (Map String Int) ()
  forwardScope = mapM_ $ \case
    S.Block b -> return () -- No recursion into blocks
    S.Declaration a b c -> do
      forwardBinder a
      mapM_ forwardBinder b
      mapM_ forwardExpression c
    S.Statement a -> do
      mapM_ forwardExpression a
  forwardExpression :: S.Expression -> State (Map String Int) ()
  forwardExpression = \case
    S.Fructose a b -> do
      mapM_ forwardBinder a
      mapM_ forwardExpression b
    S.Galactose a -> do
      mapM_ forwardExpression a
    _ -> return ()
  forwardBinder :: S.Binder -> State (Map String Int) ()
  forwardBinder (S.Binder s n) = do
    m <- get
    put $ Map.insertWith (\old new -> old) s n m

bindReferences :: Map String Int -> S.Scope -> S.Scope
bindReferences initial scope = evalState (bindScope scope) initial where
  bindScope :: S.Scope -> State (Map String Int) S.Scope
  bindScope = \case
    S.Block b -> do
      -- Store current environment on stack
      env <- get
      -- Add forward references, preferring over outer references
      put $ Map.union (forwardReferences b) env
      -- Handle body of block
      b' <- mapM bindScope b
      -- Recover current environment from before block
      put env
      return $ S.Block b'
    S.Declaration a b c -> do
      -- TODO: If the next item is a block, include its forward declarations in the environment for this declaration. For this it's probably best to modify the AST to have an S.DeclarationWithBlock.
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
      case Map.lookup s env of
        Just n' -> return $ S.Reference s n'
        Nothing -> return $ S.Reference s n
    a -> return a
  bindBinder :: S.Binder -> State (Map String Int) S.Binder
  bindBinder (S.Binder s n) = do
    env <- get
    put $ Map.insert s n env
    return $ S.Binder s n

flattenScopes :: S.Scope -> S.Scope
flattenScopes s = S.Block $ f s where
  f = \case
    S.Block a -> concatMap f a
    a -> [a]

bindingPass :: Int -> S.Scope -> S.Scope
bindingPass offset scope =
  flattenScopes $
  bindReferences (Map.fromList []) $
  numberBinders offset scope 
