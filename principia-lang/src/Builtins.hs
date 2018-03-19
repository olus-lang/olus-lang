{-# LANGUAGE LambdaCase #-}

module Builtins where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Map.Strict (fromList)

import Syntax
import Interpreter

initialEnvironment :: Environment
initialEnvironment = 
  let builtin name func = (Identifier name, ValBuiltin func) in
  fromList [
    builtin "exit" $ \x -> return (),
    builtin "print" $ \case
      [ValStr s, ret] -> do
        liftIO $ putStr s
        void $ call [ret]
      [ValInt n, ret] -> do
        liftIO $ putStr $ show n
        void $ call [ret],
    builtin "isZero" $ \case
      [ValInt n, t, f] -> call [if n == 0 then t else f],
    builtin "add" $ \case
      [ValInt a, ValInt b, ret] -> call [ret, ValInt $ a + b],
    builtin "sub" $ \case
      [ValInt a, ValInt b, ret] -> call [ret, ValInt $ a - b],
    builtin "mul" $ \case
      [ValInt a, ValInt b, ret] -> call [ret, ValInt $ a * b]
  ]
