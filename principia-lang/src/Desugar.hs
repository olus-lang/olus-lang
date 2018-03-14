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


glucase s = s

fructase s = s


-- from: step2 f: func a b c (mul n f) d e f
-- to:   step2 f: mul n f (id0: func a b c id0 d e f)

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

isGalactose :: Expr -> Bool
isGalactose (Galactose _) = True
isGalactose _ = False

findFirstGalactose :: [Expr] -> Maybe ([Expr], Expr, [Expr])
findFirstGalactose exs = case findIndex isGalactose exs of
  Nothing -> Nothing
  Just n -> Just (take n exs, exs !! n, drop (n + 1) exs)

galactaseCall :: Call -> IdGen Call
galactaseCall exs = case findFirstGalactose exs of
    Nothing -> return exs
    Just (before, Galactose call, after) -> do
      nid <- genVariable
      inner <- galactaseCall (before ++ [Var nid] ++ after)
      outer <- galactaseCall (call ++ [Fructose [nid] inner])
      return outer

galactase :: Scope -> IdGen Scope
galactase = visitCalls galactaseCall

desugar :: Scope -> Scope
desugar s = runIdGen $ galactase s
