{-# LANGUAGE LambdaCase #-}
module Closure where
  
import Data.Set (Set)
import qualified Data.Set as Set

import qualified Program as P


iter :: (a -> Set a) -> a -> Set a
iter f i = let
  i' = Set.fromList $ map f $ Set.toList i in
  if i == i' then i else iter f i'


closure :: P.Program -> Int -> Set Int
