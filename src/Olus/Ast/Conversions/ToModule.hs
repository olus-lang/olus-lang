module Olus.Ast.Conversions.ToModule where

import Olus.Ir.Module as M

import Data.Maybe (fromJust)
import Data.List (elemIndex, nub)
import qualified Data.Map.Strict as Map

import qualified Olus.Ast.Syntax as S
import qualified Olus.Ast.Passes.Binder as B
import qualified Olus.Ast.Passes.ConstantExtraction as CE
import qualified Olus.Ast.Passes.Desugar as D
import qualified Olus.Ir.Program as P
import qualified Olus.Ir.Intrinsics as I

convertExpression :: (Int -> M.Binder) -> S.Expression -> M.WithModule M.Reference
convertExpression refmap v = case v of
  S.Reference s (-1)  -> M.makeConstant $ M.External s
  S.Reference s m     -> return $ M.makeReference $ refmap m
  S.LiteralInteger n  -> M.makeConstant $ M.Integer n
  S.LiteralString s   -> M.makeConstant $ M.String s
  _ -> error "Please desugar AST before conversion"

allBinders :: S.Scope -> [S.Binder]
allBinders = \case
  S.Block b            -> concat $ map allBinders b
  S.Declaration n p _  -> n:p
  _                    -> []

convertBinders :: S.Scope -> M.WithModule (Int -> M.Binder)
convertBinders s = do
  let
    sb = allBinders s
    binMap :: S.Binder -> M.WithModule (Int, M.Binder)
    binMap (S.Binder s n) = do
      b <- makeBinder
      return (n, b)
  r <- mapM binMap sb
  return $ (Map.!) $ Map.fromList r

binderIndex :: S.Binder -> Int 
binderIndex (S.Binder _ n) = n

convertScope :: (Int -> M.Binder) -> S.Scope -> M.WithModule ()
convertScope refmap = \case
  S.Block b             -> mapM_ (convertScope refmap) b
  S.Declaration n p c   -> do
    let n' = refmap $ binderIndex n
    let p' = map (refmap . binderIndex) p
    c' <- mapM (convertExpression refmap) c
    c'' <- return c'
    addDeclaration $ M.Declaration (n':p') (M.Call c'') []
  S.Statement c         -> do
    c' <- mapM (convertExpression refmap) c
    addStatement $ M.Call c'

toModule :: S.Scope -> M.Module
toModule s = M.withModule M.empty $ do
  refmap <- convertBinders s
  m <- getModule
  convertScope refmap s
  getModule
