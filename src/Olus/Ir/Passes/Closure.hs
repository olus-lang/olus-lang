module Olus.Ir.Passes.Closure where
  
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe

import qualified Olus.Ir.Program as P

setMinus :: Set Int -> Set Int -> Set Int
setMinus a b = Set.filter (flip Set.notMember b) a

computeClosures :: P.Program -> P.Program
computeClosures prog = result where
  
  consts = P.constants prog
  decls = P.declarations prog
  names = Map.fromList $ zip (map (head . fst) decls) [0..]
  
  ids :: Int -> String
  ids n = (P.identifiers prog) !! n
  
  isBuiltin :: Int -> Bool
  isBuiltin n =  n < length consts
  
  isName :: Int -> Bool
  isName n = Map.member n names
  
  getNameIdx :: Int -> Int
  getNameIdx n = Maybe.fromJust $ Map.lookup n names
  
  removeOwnArguments :: (Int, [Int], [Int], Set Int) -> (Int, [Int], [Int], Set Int)
  removeOwnArguments (name, parameters, call, closure) = (name, parameters, call, closure') where
    closure' = setMinus closure (Set.fromList $ name:parameters)
  
  initial :: [(Int, [Int], [Int], Set Int)]
  initial = map f decls where
    f (name:parameters, call) = let
      -- Start with call
      closure = Set.fromList call
      -- Remove builtins
      closure' = Set.filter (not . isBuiltin) closure
      -- Remove own arguments
      in removeOwnArguments (name, parameters, call, closure')
  
  getClosureRec :: Set Int -> Int -> Set Int
  getClosureRec visited current = result where
    visited' = Set.union visited $ Set.singleton current
    (name, parameters, call, closure) = initial !! getNameIdx current
    -- Start with initial closure
    closure' = Set.toList closure
    -- Recursively apply getClosureRec on all names
    -- TODO: memoize
    closure'' = Set.unions $ flip map closure' $ \n ->
      if Set.member n visited then Set.empty else
      if isName n then getClosureRec visited' n else Set.singleton n
    -- Remove own arguments
    closure''' = setMinus closure'' (Set.fromList $ name:parameters)
    result = closure'''
  
  getClosure :: Int -> Set Int
  getClosure n = getClosureRec Set.empty n
  
  closures :: [Set Int]
  closures = map (getClosure . head . fst) decls
  
  result = prog { P.closures = map Set.toList closures }


showClosures :: P.Program -> String
showClosures prog = result where
  identifiers = P.identifiers prog
  declarations = P.declarations prog
  closures = P.closures prog
  iden :: Int -> String
  iden n = identifiers !! n
  showDecl :: (([Int], [Int]), [Int]) -> String
  showDecl ((name:parameters, call), closure) =
    (iden name) ++ " [" ++ (unwords $ map iden closure) ++ "] " ++ (unwords $ map iden parameters) ++ ": " ++ (unwords $ map iden call)
  result = unlines $ map showDecl $ zip declarations closures
