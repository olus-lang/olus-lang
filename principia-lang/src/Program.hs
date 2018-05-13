module Program where

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
  declarations :: [([Int],[Int])],
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

insert :: [Constant] -> Constant -> ([Constant], Int)
insert cs c = case elemIndex c cs of
  Just i -> (cs, i)
  Nothing -> (cs ++ [c], length cs)

-- TODO: Deduplicate constants
-- TODO: Sort constants by type
