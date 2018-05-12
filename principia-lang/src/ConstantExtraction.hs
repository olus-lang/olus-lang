{-# LANGUAGE LambdaCase #-}
module ConstantExtraction where

import Data.List (elemIndex, nub)
import Data.Maybe (fromJust)

import Syntax as S
import Program as P

listConstants :: S.Scope -> [P.Constant]
listConstants = nub . listBlock where
  listBlock :: S.Scope -> [P.Constant]
  listBlock = \case
    S.Block a           -> concatMap listBlock a
    S.Declaration _ _ a -> concatMap listExp a
    S.Statement a       -> concatMap listExp a
  listExp :: S.Expression -> [P.Constant]
  listExp = \case
    S.LiteralInteger n -> [P.Integer n]
    S.LiteralString s -> [P.String s]
    _ -> []

referenceConstants :: (P.Constant -> Int) -> S.Scope -> S.Scope
referenceConstants refmap = mapBlock where
  mapBlock :: S.Scope -> S.Scope
  mapBlock = \case
    S.Block a           -> S.Block $ map mapBlock a
    S.Declaration a b c -> S.Declaration a b $ map mapExp c
    S.Statement a       -> S.Statement a
  mapExp :: S.Expression -> S.Expression
  mapExp = \case
    S.LiteralInteger n -> S.Reference (show n) $ refmap $ P.Integer n
    S.LiteralString s -> S.Reference (show s) $ refmap $ P.String s
    a -> a

extractConstants :: Int -> S.Scope -> ([P.Constant], S.Scope)
extractConstants offset s = let
    consts = listConstants s
    refmap con = offset + fromJust (elemIndex con consts)
    s' = referenceConstants refmap s
  in (consts, s')
