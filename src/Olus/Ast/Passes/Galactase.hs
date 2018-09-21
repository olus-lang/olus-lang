module Olus.Ast.Passes.Galactase where

import Data.List (findIndex)

import Olus.Ast.Utils.IdGen
import qualified Olus.Ast.Syntax as S

isGalactose :: S.Expression -> Bool
isGalactose (S.Galactose _) = True
isGalactose _ = False

findFirstGalactose :: [S.Expression] -> Maybe ([S.Expression], S.Expression, [S.Expression])
findFirstGalactose exs = case findIndex isGalactose exs of
  Nothing  -> Nothing
  Just n   -> Just (take n exs, exs !! n, drop (n + 1) exs)

galactaseCall :: S.Call -> IdGen S.Call
galactaseCall exs = case findFirstGalactose exs of
  Nothing -> mapM f exs where
    f e = case e of
      S.Fructose ids call -> do
        call' <- galactaseCall call
        return $ S.Fructose ids call'
      _ -> return e
  Just (before, S.Galactose call, after) -> do
    (bind, ref) <- genVariable
    inner <- galactaseCall (before ++ [ref] ++ after)
    outer <- galactaseCall (call ++ [S.Fructose [bind] inner])
    return outer

galactase :: S.Scope -> IdGen S.Scope
galactase = visitCalls galactaseCall
