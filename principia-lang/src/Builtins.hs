module Builtins where

import Syntax
import Interpreter
import System.IO.Unsafe (unsafePerformIO)

builtin :: String -> ([Value] -> MInterpreter ()) -> MInterpreter ()
builtin name func = write name $ ValBuiltin $ BuiltinFunc (name, func)

builtinExit [] = return ()
builtinIsZero [ValInt n, t, e] = call [if n == 0 then t else e]
builtinAdd [ValInt n, ValInt m, k] = call [k, ValInt (n+m)]
builtinSub [ValInt n, ValInt m, k] = call [k, ValInt (n-m)]
builtinMul [ValInt n, ValInt m, k] = call [k, ValInt (n*m)]

-- Hack to use IO for print
pureasd :: Show a => a -> b -> b
pureasd a b = unsafePerformIO $ do 
  print a
  return b

builtinPrint :: [Value] -> MInterpreter ()
builtinPrint [a, k] = call [pureasd a k]

loadBuiltins :: MInterpreter ()
loadBuiltins = do
  builtin "exit" builtinExit
  builtin "isZero" builtinIsZero
  builtin "add" builtinAdd
  builtin "sub" builtinSub
  builtin "mul" builtinMul
  builtin "print" builtinPrint

initialEnvironment :: Environment
initialEnvironment = case execInterpreter loadBuiltins emptyEnvironment of
  Right env -> env
