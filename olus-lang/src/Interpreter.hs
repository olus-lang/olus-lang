{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Interpreter where

import Control.Monad.Reader (Reader, ask, runReader)
import Data.Map.Strict (Map)
import Data.List (elemIndex)
import qualified Data.Map.Strict as Map
import System.IO.Unsafe (unsafePerformIO)

import qualified Program as P

import Debug.Trace (trace)

data Value
  = Intrinsic Int
  | Closure Int [Value]
  | Integer Integer
  | String String
  deriving (Eq, Ord, Show)

constantToValue :: P.Constant -> Value
constantToValue = \case
  P.Integer n -> Integer n
  P.String s -> String s
  P.Intrinsic n -> Intrinsic n
  P.Closure _ _ -> undefined -- fail "Constant closures not yet supported!" -- TODO

interpretInstrinsic :: Int -> [Value] -> [Value]
interpretInstrinsic intNum args = f args where
  exit [] = []
  print_ [Integer n, r] = unsafePerformIO $ do
    putStr $ show n
    return [r]
  print_ [String s, r] = unsafePerformIO $ do
    putStr s
    return [r]
  isZero [Integer n, t, e] = if n == 0 then [t] else [e]
  add [Integer a, Integer b, r] = [r, Integer $ a + b]
  mul [Integer a, Integer b, r] = [r, Integer $ a * b]
  sub [Integer a, Integer b, r] = [r, Integer $ a - b]
  input [r] = unsafePerformIO $ do
    s <- getLine
    return [r, String s]
  parseInt [String s, r] = [r, Integer $ read s]
  f = case intNum of
    0 -> exit
    1 -> print_
    2 -> isZero 
    3 -> add
    4 -> mul
    5 -> sub
    6 -> fail "input"
    7 -> fail "parseInt"

getConstants :: Reader P.Program (Map Int Value)
getConstants = do
  prog <- ask
  let constants = P.constants prog
  let values = map constantToValue constants
  let ids = [0..length values - 1]
  return $ Map.fromList $ zip ids values

getClosure :: Int -> Reader P.Program ([Int], [Int], [Int])
getClosure n = do
  prog <- ask
  let (procedure, call) = P.declarations prog !! n
  let closure = P.closures prog !! n
  return (procedure, closure, call)

getDeclIndex :: Int -> Reader P.Program Int
getDeclIndex n = do
  prog <- ask
  let declNames = map (head . fst) $ P.declarations prog
  case elemIndex n declNames of
    Just i -> return i
    Nothing -> fail "Symbol is not a name"

createClosure :: Int -> Map Int Value -> Reader P.Program Value
createClosure n env = do
  i <- getDeclIndex n
  (procedure, closure, call) <- getClosure i
  cv <- mapM (flip getValue env) closure
  return $ Closure i cv

getValue :: Int -> Map Int Value -> Reader P.Program Value
getValue n env = do
  constants <- getConstants
  let env' = Map.union constants env
  case Map.lookup n env' of
    Just v  -> return v
    Nothing -> createClosure n env'

getValues :: [Int] -> Reader P.Program [Value]
getValues n = mapM (flip getValue Map.empty) n

singleStep :: [Value] -> Reader P.Program [Value]
singleStep values = case head values of
  Intrinsic n -> return $ interpretInstrinsic n $ tail values
  Closure n cv -> do
    constants <- getConstants
    (procedure, closure, call) <- getClosure n
    let procvals = Map.fromList $ zip procedure values
    let closvals = Map.fromList $ zip closure cv
    let env = Map.unions [constants, procvals, closvals]
    mapM (flip getValue env) call
  _ -> fail "Expression can not be evaluated"

runValues :: [Value] -> Reader P.Program [Value]
runValues [Intrinsic 0] = return [Intrinsic 0]
runValues a = do
  a' <- singleStep a
  runValues $ trace (show a') a'

run :: P.Program -> IO ()
run prog = flip mapM_ (P.statements prog) $ \statement -> do
  putStrLn $ "Exec: " ++ show statement
  let values = runReader (getValues statement) prog
  putStrLn $ "Exec: " ++ show values
  let values' = runReader (runValues values) prog
  putStrLn $ "Exec: " ++ show values'
  putStrLn "Done!"
