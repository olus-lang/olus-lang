module Olus.Ir.Intrinsics where

import Data.List (elemIndex)

import Olus.Ir.Types (Type(Integer, String, Closure))

intrinsics :: [String]
intrinsics = ["exit", "print", "isZero", "add", "mul", "sub", "input", "parseInt"]

intrinsicIndex :: String -> Maybe Int
intrinsicIndex = flip elemIndex intrinsics

-- TODO: Some intrinsics (add, mul, sub) can be constant-propagated

intrinsicTypes :: [Type]
intrinsicTypes = [
    Closure [Integer, Closure [], Closure []], -- isZero
    Closure [String, Closure []], -- print
    Closure [Integer, Integer, Closure [Integer]], -- add
    Closure [Integer, Integer, Closure [Integer]], -- mul
    Closure [Integer, Integer, Closure [Integer]], -- sub
    Closure [], -- exit
    Closure [Closure [String]], -- input
    Closure [String, Closure [Integer]] -- parseInt
  ]
