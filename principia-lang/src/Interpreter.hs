module Interpreter where

-- https://stackoverflow.com/questions/16970431/implementing-a-language-interpreter-in-haskell

import Prelude hiding (lookup, insert, read)
import Data.Map (Map, lookup, insert, fromList, union, empty)
import Control.Monad.State (StateT, get, put, execState, evalStateT)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Identity (Identity, runIdentity)
import Syntax

-- Interpreter state
data Value
  = ValInt Integer
  | ValStr String
  | ValClosure Environment [Identifier] [Expr]
  | ValBuiltin BuiltinFunc
  deriving (Eq, Ord, Show)

type Environment = Map Identifier Value

-- Interpreter Monads
type MInterpreterT e m a = StateT Environment (ExceptT e m) a

runInterpreterT :: (Monad m) => MInterpreterT e m a -> Environment -> m (Either e a)
runInterpreterT m = runExceptT . evalStateT m

type MInterpreter a = MInterpreterT String Identity a

runInterpreter :: MInterpreter a -> Environment -> Either String a
runInterpreter m = runIdentity . runInterpreterT m

-- TODO: Add IO monads? Allow builtins to add them?

-- Read a variable from the environment
read :: Identifier -> MInterpreter Value
read id = do
  env <- get
  case lookup id env of
    Just value -> return value
    Nothing -> throwError "identifier undefined"

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

-- Builtin function type
newtype BuiltinFunc = BuiltinFunc (String, [Value] -> MInterpreter ())
instance Eq BuiltinFunc where
  BuiltinFunc (a, f) == BuiltinFunc (b, g) = a == b
instance Ord BuiltinFunc where
  BuiltinFunc (a, f) `compare` BuiltinFunc (b, g) = a `compare` b
instance Show BuiltinFunc where
  show (BuiltinFunc (a, f)) = "<" ++ a ++ ">"
  

builtin :: String -> ([Value] -> MInterpreter ()) -> MInterpreter ()
builtin name func = write name $ ValBuiltin $ BuiltinFunc (name, func)

builtinExit [] = return ()
builtinIsZero [ValInt n, t, e] = call [if n == 0 then t else e]
builtinAdd [ValInt n, ValInt m, k] = call [k, ValInt (n+m)]
builtinSub [ValInt n, ValInt m, k] = call [k, ValInt (n-m)]
builtinMul [ValInt n, ValInt m, k] = call [k, ValInt (n*m)]

loadBuiltins :: MInterpreter ()
loadBuiltins = do
  builtin "exit" builtinExit
  builtin "isZero" builtinIsZero
  builtin "add" builtinAdd
  builtin "sub" builtinSub
  builtin "mul" builtinMul

initialEnvironment :: Environment
initialEnvironment = case runInterpreter (do loadBuiltins; get) empty of
  Right env -> env

executeStatements :: Environment -> [Statement] -> Either String Environment
executeStatements env statements = runInterpreter (do mapM exec statements; get) env
