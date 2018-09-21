module Olus.Ast.Utils.IdGen where

import Control.Monad.State

import qualified Olus.Ast.Syntax as S

type IdGen = State Int

runIdGen :: S.Scope -> IdGen a -> a
runIdGen s f = evalState f $ maxBinderNumber s

runIdGen' :: (S.Scope -> IdGen a) -> S.Scope -> a
runIdGen' f s = evalState (f s) $ maxBinderNumber s

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
