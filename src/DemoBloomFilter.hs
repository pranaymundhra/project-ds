{-# LANGUAGE OverloadedStrings #-}

module DemoBloomFilter where

import BloomFilter
import Control.Applicative
import Control.Monad (replicateM)
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
      sample (errorThreshold' b passwordList 0.22 passGen)

-- Oh no! It's too good! We can't be having that. Let's sabotage it.

reallybadhash :: Hash String
reallybadhash = Hasher 5000 ((`mod` 5000) . fromIntegral . murmur3 5736539 . BS'.pack)

-- note: weird behavior with arbitrary instead of passGen. Investigation required.
passGen :: Gen String
passGen = do
  length <- choose (3, 8) -- You can adjust the length
  Control.Monad.replicateM length (elements alphanumericChars)
  where
    alphanumericChars :: String
    alphanumericChars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']

badhash :: Hash String
badhash = Hasher 40000 ((`mod` 40000) . fromIntegral . murmur3 5736539 . BS'.pack)

badHashGen :: Gen (Hash String)
badHashGen = fmap (\i -> Hasher 40000 ((`mod` 40000) . fromIntegral . murmur3 (fromIntegral i) . BS'.pack)) (chooseInt (1, 1000))

-- >>> generate badHashGen
-- Hasher with maxHashed = 5000

-- true is roughly .22 for badhash

finale :: Hash String -> IO ()
finale h = do
  csvData <- BL.readFile "src/common_passwords.csv"
  case decode NoHeader csvData of
    Left err -> error "error"
    Right v -> do
      let passwordList = foldr (\(p, x1 :: Int, x2 :: Int, x3 :: Int, x4 :: Int, x5 :: Int, x6 :: Int, x7 :: Int, x8 :: Int) acc -> p : acc) [] v
          b = fromList [h] passwordList
          g = calibrateHashFunctions' passwordList 40000 0.225 badHashGen passGen b
          xxx = do
            a <- g
            errorThreshold' a passwordList 0.225 passGen
      sample xxx
