module Intrinsics where

import Data.List (elemIndex)

intrinsics :: [String]
intrinsics = ["isZero","add","print","mul","sub","exit","input","parseInt"]

intrinsicIndex :: String -> Maybe Int
intrinsicIndex = flip elemIndex intrinsics

-- TODO: Some intrinsics (add, mul, sub) can be constant-propagated
