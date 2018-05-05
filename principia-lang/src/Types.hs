
import Data.Map.Strict
import Syntax

-- http://dev.stephendiehl.com/fun/006_hindley_milner.html

data Type
  = Sentinal
  | Closure [Type]
  deriving (Show, Eq, Ord)

newtype TypeEnv = TypeEnv (Map Identifier Type)
