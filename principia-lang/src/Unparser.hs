module Unparser where 

import Syntax

-- Oddly, Haskell has no replace
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace [] _ _ = []
replace s find repl =
  if take (length find) s == find
    then repl ++ replace (drop (length find) s) find repl
    else head s : replace (tail s) find repl

parens :: [String] -> String
parens s = "(" ++ unwords s ++ ")"

unparseExpr :: Expr -> String
unparseExpr (Var x) = show x
unparseExpr (LitInt n) = show n
unparseExpr (LitStr s) = show s
unparseExpr (Fructose ids exs) = parens $ map show ids ++ ":" : map unparseExpr exs
unparseExpr (Galactose exs) = parens $ map unparseExpr exs

indent :: String -> String
indent s = "  " ++ replace s "\n" "\n  "

unparse' :: Scope -> String
unparse' (Block x) =
  unlines $ map (indent . unparse') x
unparse' (Declaration name ids exs) =
  unwords (map show $ name : ids) ++ unwords (":" : map unparseExpr exs)
unparse' (Statement exs) =
  unwords $ map unparseExpr exs

unparse :: Scope -> String
unparse (Block x) = unlines $ map unparse' x
unparse x = unparse' x
