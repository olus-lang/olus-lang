module Unparser where 

import Syntax

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace [] _ _ = []
replace s find repl =
    if take (length find) s == find
        then repl ++ (replace (drop (length find) s) find repl)
        else [head s] ++ (replace (tail s) find repl)

unparseExpr :: Expr -> String
unparseExpr (Var x) = x
unparseExpr (LitInt n) = show n
unparseExpr (LitStr s) = show s
unparseExpr (Fructose ids exs) =
  "(" ++ unwords ids ++ unwords (":" : map unparseExpr exs) ++ ")"
unparseExpr (Galactose exs) = "(" ++ unwords (map unparseExpr exs) ++ ")"

indent :: String -> String
indent s = "  " ++ replace s "\n" "\n  "

unparse :: Scope -> String
unparse (Block x) =
  unlines $ map (indent . unparse) x
unparse (Declaration name ids exs) =
  unwords (name :ids) ++ unwords (":" : map unparseExpr exs)
unparse (Statement exs) =
  unwords $ map unparseExpr exs
