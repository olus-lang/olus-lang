{-# LANGUAGE LambdaCase #-}

module Builtins where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Map.Strict (fromList)
import System.IO (hFlush, stdout)

import Syntax
import Interpreter hiding (read)

initialEnvironment :: Environment
initialEnvironment = 
  let builtin name func = (Identifier name, ValBuiltin func) in
  fromList [
    builtin "exit" $ \case
      [] -> return (),
    builtin "print" $ \case
      [ValStr s, ret] -> do
        liftIO $ putStr s
        liftIO $ hFlush stdout
        void $ call [ret]
      [ValInt n, ret] -> do
        liftIO $ putStr $ show n
        liftIO $ hFlush stdout
        void $ call [ret],
    builtin "input" $ \case
      [ret] -> do
        input <- liftIO getLine
        void $ call [ret, ValStr input],
    builtin "parseInt" $ \case
      [ValStr s, ret] -> call [ret, ValInt $ read s],
    builtin "isZero" $ \case
      [ValInt n, t, f] -> call [if n == 0 then t else f],
    builtin "add" $ \case
      [ValInt a, ValInt b, ret] -> call [ret, ValInt $ a + b],
    builtin "sub" $ \case
      [ValInt a, ValInt b, ret] -> call [ret, ValInt $ a - b],
    builtin "mul" $ \case
      [ValInt a, ValInt b, ret] -> call [ret, ValInt $ a * b]
  ]
