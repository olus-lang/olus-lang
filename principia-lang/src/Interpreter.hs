module Interpreter where

import Prelude hiding (lookup, insert, read)
import Data.Map.Strict (Map, lookup, insert, fromList, union, empty)
import Control.Monad (void)
import Control.Monad.State (StateT, MonadState, get, put, execStateT, evalStateT)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.IO.Class (MonadIO, liftIO )
import Syntax
import Unparser (unparseExpr)

-- Interpreter state
data Value
  = ValInt Integer
  | ValStr String
  | ValFunction [Identifier] [Expr]
  | ValClosure Environment [Identifier] [Expr]
  | ValBuiltin ([Value] -> MInterpreter ())

instance Show Value where
  show x = case x of
    ValInt n                -> show n
    ValStr s                -> show s
    ValFunction (name:_) _  -> "<func " ++ show name ++ ">"
    ValClosure _ (name:_) _ -> "<closure " ++ show name ++ ">"
    ValBuiltin _            -> "<builtin>"

type Environment = Map Identifier Value

type MInterpreter a = StateT Environment (ExceptT String IO) a

-- Interpreter Monads
runInterpreter :: Scope -> Environment -> IO (Either String Environment)
runInterpreter prog env = runExceptT $ execStateT (exec prog) env

emptyEnvironment :: Environment
emptyEnvironment = empty

-- Read a variable from the environment
read :: Identifier -> MInterpreter Value
read id = do
  env <- get
  case lookup id env of
    Just value -> return value
    Nothing -> throwError $ "identifier undefined: " ++ show id

-- Write a variable to the environment
write :: Identifier -> Value -> MInterpreter ()
write id val = do
  env <- get
  put $ insert id val env

-- Reduce an expression to its value
eval :: Expr -> MInterpreter Value
eval e = case e of
  LitInt n -> return $ ValInt n
  LitStr s -> return $ ValStr s
  Var id   -> do
    val <- read id
    case val of
      ValFunction funargs callargs -> do
        env <- get
        return $ ValClosure env funargs callargs
      _ -> return val
  _ -> throwError "Sugar not supported"

-- Execute a call by value
call :: [Value] -> MInterpreter ()
call (func:args) = case func of
  ValClosure env funcargs body -> do
    put $ union (fromList $ zip funcargs (func:args)) env
    mapM eval body >>= call
  ValBuiltin f -> f args
  _ -> throwError "Value is not callable"

-- Execure a statement
exec :: Scope -> MInterpreter ()
exec x = case x of
  Block statements           -> mapM_ exec statements
  Statement args             -> mapM eval args >>= call
  Declaration name ids exprs -> write name $ ValFunction (name:ids) exprs
