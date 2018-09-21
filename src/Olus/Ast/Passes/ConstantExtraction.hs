module Olus.Ast.Passes.ConstantExtraction where

import Data.List (elemIndex, nub)
import Data.Maybe (fromJust)

import Olus.Ast.Syntax as S

-- NOTE: It will bind any unboud references that match intrinsics
listConstants :: (String -> Bool) -> S.Scope -> [S.Expression]
listConstants isBuiltin = nub . listBlock where
  
  listBlock :: S.Scope -> [S.Expression]
  listBlock = \case
    S.Block a            -> concatMap listBlock a
    S.Declaration _ _ a  -> concatMap listExp a
    S.Statement a        -> concatMap listExp a
  
  listExp :: S.Expression -> [S.Expression]
  listExp ex = case ex of
    S.LiteralInteger _  -> [ex]
    S.LiteralString _   -> [ex]
    S.Reference s (-1)  -> if isBuiltin s then [ex] else []
    _                   -> []

referenceConstants :: (S.Expression -> Maybe Int) -> S.Scope -> S.Scope
referenceConstants refmap = mapBlock where
  
  mapBlock :: S.Scope -> S.Scope
  mapBlock = \case
    S.Block a            -> S.Block $ map mapBlock a
    S.Declaration a b c  -> S.Declaration a b $ map mapExp c
    S.Statement a        -> S.Statement $ map mapExp a
  
  mapExp :: S.Expression -> S.Expression
  mapExp ex = case ex of
    S.LiteralInteger n  -> S.Reference (show n) $ fromJust $ refmap ex
    S.LiteralString s   -> S.Reference (show s) $ fromJust $ refmap ex
    S.Reference s (-1)  -> case refmap ex of
      Just i   -> S.Reference s i
      Nothing  -> S.Reference s (-1)
    a                   -> a

extractConstants :: (String -> Bool) -> S.Scope -> ([S.Expression], S.Scope)
extractConstants isBuiltin s = let
    consts = listConstants isBuiltin s
    s' = S.renumber' (length consts +) s -- Shift indices up
    refmap con = elemIndex con consts
    s'' = referenceConstants refmap s'
  in (consts, s'')
