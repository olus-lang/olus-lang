module Desugar where

import Data.List (findIndex)

import Syntax

glucase s = s

fructase s = s


-- from: step2 f: func a b c (mul n f) d e f
-- to:   step2 f: mul n f (id0: func a b c id0 d e f)

visitCalls :: (Call -> Call) -> Scope -> Scope
visitCalls visitor s = case s of
  Block block -> Block $ map (visitCalls visitor) block
  Declaration name ids call -> Declaration name ids $ visitor call
  Statement call -> Statement $ visitor call

isGalactose :: Expr -> Bool
isGalactose (Galactose _) = True
isGalactose _ = False

findFirstGalactose :: [Expr] -> Maybe ([Expr], Expr, [Expr])
findFirstGalactose exs = case findIndex isGalactose exs of
  Nothing -> Nothing
  Just n -> Just (take n exs, exs !! n, drop (n + 1) exs)

galactaseCall :: Call -> Call
galactaseCall exs = case findFirstGalactose exs of
  Nothing -> exs
  Just (before, Galactose call, after) ->
    galactaseCall (call ++ [Fructose ["id0"] (galactaseCall (before ++ [Var "id0"] ++ after))])

galactase :: Scope -> Scope
galactase = visitCalls galactaseCall
