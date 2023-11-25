module BloomFilter
  (
  )
where

import Control.Applicative qualified as IntMap
import Data.IntMap (IntMap)
import HashFunction (Hash, genBoundedIntHasher)
import System.Random (StdGen)
import System.Random qualified as Random (mkStdGen, randomIO, uniform, uniformR)

mkStdGen :: Int -> StdGen
mkStdGen = Random.mkStdGen . (* (3 :: Int) ^ (20 :: Int))

-- TODO AN IMPORTANT DECISION: DO WE WANT TO USE A SET OR A MAP?

data BloomFilter a = Filter
  { max :: a,
    internalMap :: IntMap Bool,
    hashFunctions :: [Hash a]
  }

create :: [Hash a] -> BloomFilter a
create = undefined

-- empty bloomfilter

fromList :: [Hash a] -> [a] -> BloomFilter a
fromList = undefined

insert :: a -> BloomFilter a
insert = undefined

refineHashFunction :: [Hash a] -> BloomFilter a -> BloomFilter a
refineHashFunction = undefined

-- add hash functions to existing bloom filter

exists :: a -> Bool
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

instance (Eq a) => Eq (BloomFilter a) where
  (==) :: BloomFilter a -> BloomFilter a -> Bool
  a == b = undefined

-- EQUALITY WILL DEPEND ON THE INTERNAL DATA STRUCTURE
