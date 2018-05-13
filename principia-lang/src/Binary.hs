{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
module Binary where

import Prelude hiding (writeFile, readFile)
import Data.List (unfoldr)
import Data.Bits (shiftR)
import Data.Word (Word8)
import Data.Binary.Get (Get, getWord8, getByteString, runGet)
import Data.Binary.Put (Put, putWord8, runPut)
import Data.ByteString (unpack)
import Data.ByteString.Lazy (ByteString, toStrict, writeFile, readFile)
import Data.ByteString.Builder (toLazyByteString, lazyByteStringHex)
import qualified Data.ByteString.Char8 as BSC
import Data.Char (ord, chr)

import qualified Program as P

class Serial a where
  get :: Get a
  put :: a -> Put

toInt :: [Word8] -> Int
toInt [] = 0
toInt [x] = fromIntegral x
toInt (x:xs) = fromIntegral x * 256 + toInt xs 

fromInt :: Int -> [Word8]
fromInt = unfoldr step
  where
    step :: Int -> Maybe (Word8, Int)
    step i = Just (fromIntegral i, shiftR i 8)

----------------------
---  Varint encoding -
----------------------
instance Serial Int where
  get = do
    h <- getWord8
    case h of
      _ | h < 128 -> return $ fromIntegral h
        | h < 192 -> do ; x <- readBytes 1 h ; return (x - 32640)
        | h < 224 -> do ; x <- readBytes 2 h ; return (x - 12566400)
        | h < 240 -> do ; x <- readBytes 3 h ; return (x - 3755982720)
        | h < 248 -> do ; x <- readBytes 4 h ; return (x - 1030521601920)
        | h < 252 -> do ; x <- readBytes 5 h ; return (x - 272644253400960)
        | otherwise -> error "Not implemented"
    where
      readBytes :: Int -> Word8 -> Get Int
      readBytes n x = do
        xs <- getByteString n
        return $ toInt $ x : unpack xs
  put a | a <             0 = error "Negative numbers not supported"
        | a <           128 = writeBytes 1 a
        | a <         16512 = writeBytes 2 (a + 32640)
        | a <       2113664 = writeBytes 3 (a + 12566400)
        | a <     270549120 = writeBytes 4 (a + 3755982720)
        | a <   34630287488 = writeBytes 5 (a + 1030521601920)
        | a < 4432676798592 = writeBytes 6 (a + 272644253400960)
        | otherwise = error "Not implemented"
    where
      writeBytes :: Int -> Int -> Put
      writeBytes b n = mapM_ putWord8 $ reverse $ take b $ fromInt n

putInt :: Int -> Put
putInt = put

instance Serial Integer where
  get = fmap toInteger (get :: Get Int)
  put = putInt . fromInteger

instance Serial Char where 
  get = fmap chr get
  put = put . ord

getFixedArray :: Serial a => Int -> Get [a]
getFixedArray = \case
  0 -> return []
  n -> do
    e <- get
    r <- getFixedArray (n - 1)
    return $ e : r

putFixedArray :: Serial a => [a] -> Put
putFixedArray = mapM_ put

instance Serial a => Serial [a] where 
  get = do
    l <- get
    getFixedArray l
  put a = do
    put $ length a
    mapM_ put a

instance Serial P.Constant where
  put = \case
    P.Integer a -> put $ a * 4
    P.String a -> do
      put $ length a * 4 + 1
      putFixedArray a
    P.Intrinsic a -> put $ a * 4 + 2
    P.Closure a b -> do
      put $ a * 4 + 3
      put b
  get = do
    flag <- get :: Get Int
    let x = quot flag 4 in case mod flag 4 of
      0 -> return $ P.Integer $ toInteger x
      1 -> do
        value <- getFixedArray x
        return $ P.String value
      2 -> return $ P.Intrinsic x
      3 -> do
        value <- get
        return $ P.Closure x value
      _ -> error "Invalid file"

instance Serial ([Int], [Int]) where
  put (a, b) = do
    put $ length a
    put b
  get = do
    a <- get -- TODO: Generate consecutive ids
    b <- get
    return (replicate a 0, b)

instance Serial P.Program where
  put p = do
    -- This encodes as "Olus" in ascii
    mapM_ putInt [0x4F, 0x6C, 0x75, 0x73]
    
    -- Format version major, minor
    putInt 0 -- Major format version
    putInt 0 -- Minor format version
    
    -- Program
    put $ P.constants p
    put $ P.declarations p
    put $ P.statements p
    
  get = do
    -- Magic
    -- TODO: Check magic
    _ <- getWord8
    _ <- getWord8
    _ <- getWord8
    _ <- getWord8
    
    -- Version
    -- TODO: Check version
    _ <- get :: Get Int
    _ <- get :: Get Int
    
    consts <- get
    decs <- get
    stmts <- get
    
    return $ P.empty {
      --identifiers = ids,
      P.constants = consts,
      P.declarations = decs,
      P.statements = stmts
    }

encode :: Serial a => a -> ByteString
encode = runPut . put

decode :: Serial a => ByteString -> a
decode = runGet get

encodeFile :: Serial a => FilePath -> a -> IO ()
encodeFile f v = writeFile f (encode v)

decodeFile :: Serial a => FilePath -> IO a
decodeFile f = do
  result <- readFile f
  return $ decode result
  
toHex :: ByteString -> String
toHex b = BSC.unpack $ toStrict $ toLazyByteString $ lazyByteStringHex b
