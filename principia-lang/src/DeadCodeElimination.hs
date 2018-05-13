module DeadCodeElimination where

import Data.Set (Set)
import qualified Data.Set as Set

import qualified Program as P

setMinus :: (Ord a) => Set a -> Set a -> Set a
setMinus a b = Set.filter (`Set.notMember` b) a

converge :: (Eq a) => (a -> a) -> a -> a
converge f a = let a' = f a in if a == a' then a else converge f a'


scanStatements :: P.Program -> Set Int
scanStatements prog = Set.unions $ map Set.fromList $ P.statements prog

recurseDeclarations :: P.Program -> Set Int -> Set Int
recurseDeclarations prog set = foldr reducer set $ P.declarations prog where
  reducer :: ([Int], [Int]) -> Set Int -> Set Int
  reducer (a:_, c) s = if Set.member a s then Set.union s $ Set.fromList c else s
  reducer _ _ = error "Procedure can not be empty"

allDeclarations :: P.Program -> Set Int
allDeclarations prog = Set.fromList $ map f $ P.declarations prog where
  f (a:_, _) = a
  f _ = error "Procedure can not be empty"

usedDeclarations :: P.Program -> Set Int
usedDeclarations prog = Set.intersection
  (allDeclarations prog)
  (converge (recurseDeclarations prog) (scanStatements prog))

unusedDeclarations :: P.Program -> Set Int
unusedDeclarations prog = setMinus (allDeclarations prog) (usedDeclarations prog)

removeDeclarations :: Set Int -> P.Program -> P.Program
removeDeclarations set prog = prog {
    P.declarations = filter f $ P.declarations prog
  } where
    f :: ([Int], [Int]) -> Bool
    f (a:_, _) = Set.notMember a set
    f _ = error "Procedure can not be empty"

removeDeadDeclarations :: P.Program -> P.Program 
removeDeadDeclarations prog = removeDeclarations (unusedDeclarations prog) prog

-- TODO: Remove dead constants
