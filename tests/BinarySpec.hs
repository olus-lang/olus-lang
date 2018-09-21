{-# LANGUAGE LambdaCase #-}
module BinarySpec where

import Binary (Serial, encode, decode)
import Test.QuickCheck
import Test.Hspec (Spec, describe, it)

decenc :: (Serial a, Eq a) => a -> Bool
decenc a = decode (encode a) == a

--encdec :: (Serial a) => ByteString -> bool
--encdec b = encode (decode b) :: a == b

spec :: Spec
spec = do
  describe "Int" $ do
    it "should store non-negative ints" $ property $ \case
      (NonNegative i) -> decenc (i :: Int)
    it "should store 128" $ 
      decenc (16512::Int)
    it "should store large non-negative ints" $ property $ \case
      (NonNegative (Large i)) -> decenc (i :: Int)
