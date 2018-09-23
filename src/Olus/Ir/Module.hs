module Olus.Ir.Module where

import Control.Monad.State
import Data.List (find, findIndices)

data Binder = Binder Int
  deriving (Show, Eq, Ord)

data Reference = Reference Int
  deriving (Show, Eq, Ord)

data Value
  = Integer Integer
  | String String
  | External String
  | Closure [Reference]
  deriving (Show, Eq, Ord)

data Constant = Constant Binder Value
  deriving (Show, Eq, Ord)

data Call = Call [Reference]
  deriving (Show, Eq, Ord)

data Declaration = Declaration [Binder] Call [Reference]
  deriving (Show, Eq, Ord)

data Module = Module {
  _counter     :: Int,
  constants    :: [Constant],
  declarations :: [Declaration],
  statements   :: [Call]
} deriving (Show, Eq, Ord)

empty :: Module
empty = Module {
  _counter     = 0,
  constants    = [],
  declarations = [],
  statements   = []
}

--
-- State monad for Module
--

type WithModule = State Module

withModule :: Module -> WithModule a -> a
withModule m f = evalState f m

getModule :: WithModule Module
getModule = get

setModule :: Module -> WithModule ()
setModule m = put m

--
-- Binders and references
--

makeBinder :: WithModule Binder
makeBinder = do
  m <- getModule
  let c = 1 + (_counter m)
  put $ m { _counter = c }
  return $ Binder c

makeReference :: Binder -> Reference
makeReference (Binder n) = Reference n

references :: Binder -> Reference -> Bool
references (Binder n) (Reference m) = n == m

data LookupResult
  = NotFound
  | FoundConstant Constant
  | FoundDeclaration Declaration Int

-- Find the binding site of a reference
lookupReference :: Reference -> WithModule (Maybe LookupResult)
lookupReference r = do
  m <- getModule
  return $ zeroOrOne $ result m
  where 
    lrConst :: Constant -> Maybe LookupResult
    lrConst c@(Constant binder value) =
      if references binder r then Just $ FoundConstant c else Nothing
    lrDecl :: Declaration -> [Maybe LookupResult]
    lrDecl d@(Declaration binders _ _) = 
      map (\i -> Just $ FoundDeclaration d i) $ findIndices (flip references r) binders
    result :: Module -> [Maybe LookupResult]
    result m = concat $ (map lrConst $ constants m):(map lrDecl $ declarations m)
    -- Utility function to pick at most one from a list of Maybe's
    zeroOrOne :: [Maybe a] -> Maybe a
    zeroOrOne []     = Nothing
    zeroOrOne (x:xs) = case x of
      Nothing -> zeroOrOne xs
      Just a  -> case zeroOrOne xs of
        Nothing -> Just a
        Just _  -> error "More than one result"

-- Replace all occurances of a reference with another
replaceReference :: Reference -> Reference -> WithModule ()
replaceReference a b = 
  do
    m <- getModule
    put $ m {
      constants = map mapConst (constants m),
      declarations = map mapDecl (declarations m)
    }
    where
      mapList :: [Reference] -> [Reference]
      mapList = map (\r -> if r == a then b else r)
      mapConst :: Constant -> Constant
      mapConst (Constant b (Closure v)) =
        Constant b (Closure $ mapList v)
      mapConst c = c
      mapDecl :: Declaration -> Declaration
      mapDecl (Declaration b (Call r) co) =
        Declaration b (Call $ mapList r) (mapList co)

--
-- Constants, Declarations and Statements
--

addConstant :: Constant -> WithModule ()
addConstant c = do
  m <- getModule
  let cs = constants m
  setModule $ m { constants = cs ++ [c] }

addDeclaration :: Declaration -> WithModule ()
addDeclaration d = do
  m <- getModule
  let ds = declarations m
  setModule $ m { declarations = ds ++ [d] }
  
addStatement :: Call -> WithModule ()
addStatement s = do
  m <- getModule
  let ss = statements m
  setModule $ m { statements = ss ++ [s] }

-- Returns a reference to the constant of that value or creates a new one.
makeConstant :: Value -> WithModule Reference
makeConstant v = do
  let existing (Constant _ v') = v == v' 
  m <- getModule
  case find existing (constants m) of
    Just (Constant b _) ->
      return $ makeReference b
    Nothing -> do
      b <- makeBinder
      addConstant $ Constant b v
      return $ makeReference b
