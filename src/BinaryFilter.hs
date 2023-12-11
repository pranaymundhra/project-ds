module BinaryFilter where

import BloomFilter
import Data.IntSet (IntSet, empty, insert, member)
import HashFunction (Hash (Hasher), Seed (Se), customShow, exampleHash, genBoundedIntHasher)
import Test.QuickCheck

-- an example of how simply a relatively well dispersed function into the integers
-- that is NOT injective would work with our bloom filter.
-- For this, let's use binary

-- an implementation of binary numbers in standard notation (i.e. the only number
-- that starts with a 0 is 0 itself, so each number has a unique representation)

data BinaryElem = Zero | One

data BinaryNum = Single BinaryElem | Cons BinaryElem BinaryNum

data MinBinaryNum = MinZero | MinOne | Tail BinaryNum

-- tail means 1 followed by the BinaryNum

-- simply multiply each number by its index + 1
-- contrast this with the "obvious" injective function we could have defined that would map
-- each binary number to its equivalent base 10 value.

instance CustomMap MinBinaryNum where
  convert :: MinBinaryNum -> Int
  convert MinZero = 0
  convert MinOne = 1
  convert (Tail b) = go b 2 1
    where
      go :: BinaryNum -> Int -> Int -> Int
      go b multiplier sum = case b of
        Single Zero -> sum
        Single One -> sum + multiplier
        Cons Zero binNum -> go binNum (multiplier + 1) sum
        Cons One binNum -> go binNum (multiplier + 1) (sum + multiplier)

-- >>> convert (Tail (Single Zero))
-- 1

createBinary :: Int -> MinBinaryNum
createBinary i = case binaryConverter i of
  [0] -> MinZero
  [1] -> MinOne
  [] -> error "should never hit"
  x : xs -> Tail (createBinaryNum xs)

createBinaryNum :: [Int] -> BinaryNum
createBinaryNum arr = case arr of
  [] -> error "should never hit"
  x : (y : ys) -> Cons (convertElem x) (createBinaryNum (y : ys))
  [x] -> Single (convertElem x)
  where
    convertElem :: Int -> BinaryElem
    convertElem x = if x == 0 then Zero else One

binaryConverter :: Int -> [Int]
binaryConverter i = if i == 0 then [0] else go i []
  where
    go i arr =
      if i == 0
        then arr
        else case mod i 2 of
          0 -> go (div i 2) (0 : arr)
          1 -> go (div i 2) (1 : arr)
          _ -> error "mod is broken"

trueConvert :: MinBinaryNum -> Int
trueConvert m = case m of
  MinZero -> 0
  MinOne -> 1
  Tail xs -> go xs 1
    where
      go ms i = case ms of
        Single Zero -> i * 2
        Single One -> i * 2 + 1
        Cons Zero a -> go a (2 * i)
        Cons One a -> go a (2 * i + 1)

-- Example of why it is not injective:
-- 10001

-- >>> convert (Tail (Cons Zero (Cons Zero (Cons Zero (Single One)))))
-- 6

-- >>> convert (Tail (Cons One (Single One)))
-- 6

-- >>> trueConvert (Tail (Cons Zero (Cons Zero (Cons Zero (Single One)))))
-- 17

-- >>> trueConvert (Tail (Cons One (Single One)))
-- 7

instance Show MinBinaryNum where
  show :: MinBinaryNum -> String
  show m = show (trueConvert m)

instance Arbitrary MinBinaryNum where
  arbitrary :: Gen MinBinaryNum
  arbitrary = fmap createBinary (chooseInt (0, 100))
  shrink :: MinBinaryNum -> [MinBinaryNum]
  shrink m = fmap createBinary (divisorList (div i 2) [])
    where
      i = trueConvert m
      divisorList :: Int -> [Int] -> [Int]
      divisorList n arr = if n == 0 then arr else divisorList (div n 2) (n : arr)

prop_contains_consistent_bin :: MinBinaryNum -> [Hash Int] -> Bool
prop_contains_consistent_bin ibin hashes = exists i b'
  where
    b = create hashes
    b' = BloomFilter.insert i b
    i = convert ibin
