module Interpreter where

-- https://stackoverflow.com/questions/16970431/implementing-a-language-interpreter-in-haskell

import Prelude hiding (lookup, insert, read)
import Data.Map (Map, lookup, insert, fromList, union, empty)
import Control.Monad.State (StateT, get, put, execStateT, evalStateT)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Identity (Identity, runIdentity)
import Syntax

-- Interpreter state
type Environment = Map Identifier Value

data Value
  = ValInt Integer
  | ValStr String
  | ValClosure Environment [Identifier] [Expr]
  | ValBuiltin BuiltinFunc
  deriving (Eq, Ord, Show)

-- Builtin function type
newtype BuiltinFunc = BuiltinFunc (String, [Value] -> MInterpreter ())
instance Eq BuiltinFunc where
  BuiltinFunc (a, f) == BuiltinFunc (b, g) = a == b
instance Ord BuiltinFunc where
  BuiltinFunc (a, f) `compare` BuiltinFunc (b, g) = a `compare` b
instance Show BuiltinFunc where
  show (BuiltinFunc (a, f)) = "<" ++ a ++ ">"

-- Interpreter Monads
type MInterpreterT m a = StateT Environment (ExceptT String m) a

type MInterpreter a = MInterpreterT Identity a

runInterpreterT :: (Monad m) => MInterpreterT m a -> Environment -> m (Either String a)
runInterpreterT m = runExceptT . evalStateT m

execInterpreterT :: (Monad m) => MInterpreterT m a -> Environment -> m (Either String Environment)
execInterpreterT m = runExceptT . execStateT m

runInterpreter :: MInterpreter a -> Environment -> Either String a
runInterpreter m = runIdentity . runInterpreterT m

execInterpreter :: MInterpreter a -> Environment -> Either String Environment
execInterpreter m = runIdentity . execInterpreterT m

execStatements :: Environment -> [Statement] -> Either String Environment
execStatements env statements = execInterpreter (mapM exec statements) env


-- TODO: Add IO monads? Allow builtins to add them?

-- Read a variable from the environment
read :: Identifier -> MInterpreter Value
read id = do
  env <- get
  case lookup id env of
    Just value -> return value
    Nothing -> throwError $ "identifier undefined: " ++ id

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
  Var id   -> read id

-- Execute a call by value
call :: [Value] -> MInterpreter ()
call (func:args) = case func of
  ValBuiltin (BuiltinFunc (n, f)) -> f args
  ValClosure env funcargs body -> do
    put $ union (fromList $ zip funcargs (func:args)) env
    exec (Call body)

-- Execute a statement updating the environment
exec :: Statement -> MInterpreter ()
exec s = case s of
  Closure (func:args) callargs -> do
    env <- get
    write func (ValClosure env (func:args) callargs)
  Call callargs -> do
    vals <- mapM eval callargs
    call vals

emptyEnvironment :: Environment
emptyEnvironment = empty
