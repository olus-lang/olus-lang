{-# LANGUAGE LambdaCase #-}
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

unparseBinder :: S.Binder -> String
unparseBinder (S.Binder s _) = s

unparseExpr :: S.Expression -> String
unparseExpr = \case
  S.Reference s _    -> s
  S.LiteralInteger n -> show n
  S.LiteralString s  -> show s
  S.Fructose ids exs -> parens $ map unparseBinder ids ++ ":" : map unparseExpr exs
  S.Galactose exs    -> parens $ map unparseExpr exs

indent :: String -> String
indent s = "  " ++ replace s "\n" "\n  "

unparse' :: S.Scope -> String
unparse' = \case
  S.Block x -> unlines $ map (indent . unparse') x
  S.Declaration name ids exs ->
    unwords (map unparseBinder $ name : ids) ++ unwords (":" : map unparseExpr exs)
  S.Statement exs-> unwords $ map unparseExpr exs

unparse :: S.Scope -> String
unparse = \case
  S.Block x -> unlines $ map unparse' x
  x -> unparse' x
