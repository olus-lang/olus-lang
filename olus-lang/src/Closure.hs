{-# LANGUAGE LambdaCase #-}
module Closure where
  
import Data.Set (Set)
import qualified Data.Set as Set

import qualified Program as P

setMinus :: Set Int -> Set Int -> Set Int
setMinus a b = Set.filter (flip Set.notMember b) a

computeClosures :: P.Program -> P.Program
computeClosures prog = result where
  
  consts = P.constants prog
  decls = P.declarations prog
  
  isBuiltin :: Int -> Bool
  isBuiltin n =  n < length consts
  
  ownArguments :: [Set Int]
  ownArguments = map (Set.fromList . fst) decls
  
  initialMap :: [Set Int]
  initialMap = map (Set.fromList . snd) decls
  
  withoutBuiltins :: [Set Int]
  withoutBuiltins = map (Set.filter (not . isBuiltin)) initialMap
  
  removeOwnArguments :: [Set Int] -> [Set Int]
  removeOwnArguments a = map f $ zip ownArguments a where
    f :: (Set Int, Set Int) -> Set Int
    f (own, new) = setMinus new own
  
  replace :: Int -> Set Int -> Set Int -> Set Int
  replace from to set = Set.unions $ map f $ Set.toList set where
    f :: Int -> Set Int
    f n = if n == from then to else Set.singleton n
  
  closures :: [Set Int]
  closures = foldl f withoutBuiltins [0..length decls - 1] where
    f :: [Set Int] -> Int -> [Set Int]
    f set n = removeOwnArguments $ map (replace name closure) set where
      name = head . fst $ decls!!n
      closure = set!!n
  
  result = prog { P.closures = map Set.toList closures }
