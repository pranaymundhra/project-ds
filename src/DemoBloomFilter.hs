{-# LANGUAGE OverloadedStrings #-}

module DemoBloomFilter where

import BloomFilter
import Control.Applicative
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS'
import Data.ByteString.Lazy (append)
import Data.ByteString.Lazy qualified as BL
import Data.Csv
import Data.Vector qualified as V
import Data.Word (Word32, Word8)
import HashFunction
import Murmur
import Test.QuickCheck

x :: Gen Word32
x = fmap fromIntegral (chooseInt (1, 4))

y :: Gen (Hash Word8)
y = do
  a <- x
  return (Hasher (2 ^ 32) (fromIntegral . murmur3 a . BS.singleton))

z :: Gen (Hash String)
z = do
  a <- x
  return (Hasher (2 ^ 32) (fromIntegral . murmur3 a . BS'.pack))

demowords :: [String]
demowords = ["hello", "cis", "5520"]

samplehash :: Hash String
samplehash = Hasher (2 ^ 32) (fromIntegral . murmur3 1234 . BS'.pack)

myBloomFilter :: BloomFilter String
myBloomFilter = fromList [samplehash] demowords

-- >>> exists "hello" myBloomFilter
-- True

-- >>> exists "haskell" myBloomFilter
-- False

main' :: Hash String -> IO ()
main' h = do
  csvData <- BL.readFile "src/common_passwords.csv"
  case decode NoHeader csvData of
    Left err -> putStrLn err
    Right v -> do
      let passwordList = foldr (\(p, x1 :: Int, x2 :: Int, x3 :: Int, x4 :: Int, x5 :: Int, x6 :: Int, x7 :: Int, x8 :: Int) acc -> p : acc) [] v
          b = fromList [h] passwordList
      print (exists "pwd" b)

main :: Hash String -> IO ()
main h = do
  csvData <- BL.readFile "src/common_passwords.csv"
  case decode NoHeader csvData of
    Left err -> error "error"
    Right v -> do
      let passwordList = foldr (\(p, x1 :: Int, x2 :: Int, x3 :: Int, x4 :: Int, x5 :: Int, x6 :: Int, x7 :: Int, x8 :: Int) acc -> p : acc) [] v
          b = fromList [h] passwordList
      -- x = frequency [(100, arbitrary :: Gen String), (1, elements passwordList)]
      sample (errorThreshold' b passwordList 0 arbitrary)
