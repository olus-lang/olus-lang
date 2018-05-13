module Program where

import Data.Maybe (fromJust)
import qualified Data.Map.Strict as M
import Data.List (elemIndex)

data Constant
  = Integer Integer
  | String String
  | Intrinsic Int -- Closure of a built in function
  | Closure Int [Int]
  deriving (Show, Eq, Ord)

data Program = Program {

  -- Program data
  constants :: [Constant],
  declarations :: [([Int],[Int])], -- (procedure, call)
  statements :: [[Int]],
  
  -- Closures (derived from declarations)
  closures :: [[Int]],
  
  -- Debug data
  identifiers :: [String],
  
  -- Index
  index :: M.Map Int (Int, Int)
  
} deriving (Eq, Ord, Show)

empty :: Program
empty = Program {
  constants = [],
  declarations = [],
  statements = [],
  closures = [],
  identifiers = [],
  index = M.empty
}

renumber :: (Int -> Int) -> Program -> Program
renumber fn prog = prog {
    declarations = map (mapTpl (map fn) (map fn)) (declarations prog),
    statements = map (map fn) (statements prog)
  } where
    mapTpl fa fb (a, b) = (fa a, fb b)

canonicalize :: Program -> Program
canonicalize prog = renumber fn prog where
  old = [0..length (constants prog) - 1] ++ concatMap fst (declarations prog)
  fm = M.fromList $ zip old [0..]
  fn a = fromJust $ M.lookup a fm
