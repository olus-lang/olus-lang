module Unparser where 

import qualified Syntax as S

-- Oddly, Haskell has no replace
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace [] _ _ = []
replace s find repl =
  if take (length find) s == find
    then repl ++ replace (drop (length find) s) find repl
    else head s : replace (tail s) find repl

parens :: [String] -> String
parens s = "(" ++ unwords s ++ ")"

unparseExpr :: S.Expression -> String
unparseExpr (S.Reference x _) = show x
unparseExpr (S.LiteralInteger n) = show n
unparseExpr (S.LiteralInteger s) = show s
unparseExpr (S.Fructose ids exs) = parens $ map show ids ++ ":" : map unparseExpr exs
unparseExpr (S.Galactose exs) = parens $ map unparseExpr exs

indent :: String -> String
indent s = "  " ++ replace s "\n" "\n  "

unparse' :: S.Scope -> String
unparse' (S.Block x) =
  unlines $ map (indent . unparse') x
unparse' (S.Declaration name ids exs) =
  unwords (map show $ name : ids) ++ unwords (":" : map unparseExpr exs)
unparse' (S.Statement exs) =
  unwords $ map unparseExpr exs

unparse :: S.Scope -> String
unparse (S.Block x) = unlines $ map unparse' x
unparse x = unparse' x
