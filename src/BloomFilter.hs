module BloomFilter
  (
  )
where

import Control.Applicative qualified as IntMap
import Data.IntSet (IntSet)
import HashFunction (Hash, genBoundedIntHasher)
import System.Random (StdGen)
import System.Random qualified as Random (mkStdGen, randomIO, uniform, uniformR)

mkStdGen :: Int -> StdGen
mkStdGen = Random.mkStdGen . (* (3 :: Int) ^ (20 :: Int))

-- TODO AN IMPORTANT DECISION: DO WE WANT TO USE A SET OR A MAP?

data BloomFilter a = Filter
  { maxHashed :: Int,
    internalSet :: IntSet,
    hashFunctions :: [Hash a]
  }

create :: [Hash a] -> BloomFilter a
create = undefined

-- empty bloomfilter

fromList :: [Hash a] -> Int -> [a] -> BloomFilter a
fromList = undefined

insert :: a -> BloomFilter a -> BloomFilter a
insert = undefined

addHashFunctions :: [Hash a] -> BloomFilter a -> BloomFilter a
addHashFunctions = undefined

-- add hash functions to existing bloom filter

exists :: a -> BloomFilter a -> Bool
exists = undefined

errorThreshold :: BloomFilter a -> [a] -> Double -> Bool
errorThreshold = undefined

-- this function takes in a bloom filter, a list of added elements, and
-- an acceptable error threshold for false positives,
-- and checks the probability of a false positive versus that threshold
-- WE WANT TO PASS IN SOME FORM OF GENERATOR (USING RANDOM STATE)

calibrateHashFunctions :: BloomFilter a -> Double -> Double -> BloomFilter a
calibrateHashFunctions original sizeRatio = undefined

-- take in a bloom filter and a size ratio > 1 (max ratio we are willing for the output
-- bloom filter internal data structure vs elements put in). Then generate an appropriate
-- number of hash functions randomly until our false positive error is below the
-- error threshold
-- THIS ALSO NEEDS A GENERATOR

-- below this goes support for integer based hashing (state monad to create random hash functions)

-- below that goes an example of how simply a relatively well dispersed function into the integers should
-- work fine with our bloom filter.

data BinaryElem = Zero | One

data BinaryNum = Single BinaryElem | Cons BinaryElem BinaryNum

data MinBinaryNum = MinZero | MinOne | Tail BinaryNum

-- tail means 1 followed by the BinaryNum

convertBinaryToInt :: MinBinaryNum -> Int
convertBinaryToInt MinZero = 0
convertBinaryToInt MinOne = 1
convertBinaryToInt (Tail b) = go b 1 2
  where
    go :: BinaryNum -> Int -> Int -> Int
    go b multiplier sum = case b of
      Single Zero -> sum
      Single One -> sum + multiplier
      Cons Zero binNum -> go binNum (multiplier + 1) sum
      Cons One binNum -> go binNum (multiplier + 1) (sum + multiplier)

-- a "bad" (non-injective) converter from binary numbers to integers
-- multiply each number by its index + 1
