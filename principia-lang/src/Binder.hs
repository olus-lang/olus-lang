module Syntax where

import qualified Syntax as S

listIdentifiers :: S.Scope -> [String]
listIdentifiers = \case
  S.Block b           -> concatMap listIdentifiers b
  S.Declaration a b _ -> map listIdentifiers' $ a : b
  S.Statement _       -> []

listIdentifiers' :: S.Identifier -> String
listIdentifiers' (S.Identifier s) = s


newtype Path = Path [Int]
  deriving (Show, Eq, Ord)

walkIds' :: Path -> S.Scope -> [(Path, S.Identifier)]
walkIds' (Path p) = \case
  S.Block b           -> concat $ zipWith f [0..] b where
    f :: Int -> S.Scope -> [(Path, S.Identifier)]
    f a = walkIds' (Path $ a : p)
  S.Declaration c a _ -> (Path $ 0 : p, c) : zipWith f [1..] a where
    f :: Int -> S.Identifier -> (Path, S.Identifier)
    f a i = (Path $ a : p , i)
  S.Statement _       -> []

walkIds :: S.Scope -> [(Path, S.Identifier)]
walkIds = walkIds' (Path [])
