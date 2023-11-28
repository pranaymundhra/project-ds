module BloomFilter where

import Data.IntSet (IntSet)
import HashFunction (Hash (Hasher), exampleHash)
import System.Random (StdGen)
import System.Random qualified as Random (mkStdGen, uniform)

mkStdGen :: Int -> StdGen
mkStdGen = Random.mkStdGen . (* (3 :: Int) ^ (20 :: Int))

data BloomFilter a = Filter
  { maxHashed :: Int,
    internalSet :: IntSet,
    hashFunctions :: [Hash a]
  }

create :: [Hash a] -> BloomFilter a
create = undefined

-- empty bloomfilter

fromList :: [Hash a] -> [a] -> BloomFilter a
fromList = undefined

insert :: a -> BloomFilter a -> BloomFilter a
insert = undefined

addHashFunctions :: [Hash a] -> BloomFilter a -> BloomFilter a
addHashFunctions = undefined

-- add hash functions to existing bloom filter

exists :: a -> BloomFilter a -> Bool
exists = undefined

-- a generalization of StdGen
-- when restricted to Int, we want it to be StdGen

class RandomGen g a where
  next :: g -> Int -> (a, g)

-- the second param is an optional "bound"

instance RandomGen StdGen Int where
  next :: StdGen -> Int -> (Int, StdGen)
  next s b = Random.uniform s

errorThreshold :: (RandomGen m a) => BloomFilter a -> [a] -> Double -> m -> (Bool, m)
errorThreshold = undefined

-- this function takes in a bloom filter, a list of added elements, and
-- an acceptable error threshold for false positives,
-- and checks the probability of a false positive versus that threshold
-- an adversarial construction of randomgen can cause this to enter an infinite loop

calibrateHashFunctions :: (RandomGen m (Hash a)) => [a] -> Int -> Double -> m -> (BloomFilter a, m)
calibrateHashFunctions elems size threshold = undefined

-- take in a set of elements and size of output bloom filter
-- Then generate an appropriate
-- number of hash functions randomly until our false positive error is below the
-- error threshold
-- it is possible for this to enter an infinite loop

-- SUPPORT FOR INTEGER BASED HASHING

uniformInt :: StdGen -> (Int, StdGen)
uniformInt = Random.uniform

instance RandomGen StdGen (Hash Int) where
  next :: StdGen -> Int -> (Hash Int, StdGen)
  next g b = (Hasher b hashF, g'')
    where
      (x, g') = uniformInt g
      (y, g'') = uniformInt g'
      hashF h = (h * x + y) `mod` b

-- the below code type checks!
-- TODO move into inline doctests later

x :: (Bool, StdGen)
x = errorThreshold (create [exampleHash]) [] 0.1 (mkStdGen 1)

y :: (BloomFilter Int, StdGen)
y = calibrateHashFunctions [] 100 0.1 (mkStdGen 1)

-- Giving integer support means that instead of having to write some kind of generator for
-- some custom datatype, the user simply needs to define a function (ideally injective)
-- from that datatype into the integers and then we can simply use an integer bloom filter
-- If the function is not injective, false positives are more likely due to collision
-- between the images of the function

class CustomMap a where
  convert :: a -> Int

z :: (CustomMap a) => [a] -> (BloomFilter Int, StdGen)
z m = calibrateHashFunctions (fmap convert m) 100 0.1 (mkStdGen 1)

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

-- Example of why it is not injective:
-- 10001
-- >>> convert (Tail (Cons Zero (Cons Zero (Cons Zero (Single One)))))
-- 6
-- >>> 111
-- >>> convert (Tail (Cons One (Single One)))
-- 6

q :: [MinBinaryNum] -> (BloomFilter Int, StdGen)
q = z

-- NOTE any quickchecking can now be done with this more familiar type as well as the ints.
-- writing a test suite should now be easy!
-- IMPORTANT for the quickcheck, remember that the seed fed into the generator should also be randomly
-- generated

-- TODO move all below tests to test directory

newtype Seed = Se {seededVal :: Int}

-- using a custom datatype here so that we can stipulate that quickcheck's shrinking
-- doesn't touch the seed

prop_contains_consistent_int :: Int -> [Hash Int] -> Bool
prop_contains_consistent_int i hashes = exists i b'
  where
    b = create hashes
    b' = insert i b

prop_toList_consistent_int :: [Int] -> [Hash Int] -> Bool
prop_toList_consistent_int i hashes = all (`exists` b) i
  where
    b = fromList hashes i

prop_errorThreshold_zero_int :: [Int] -> [Hash Int] -> Seed -> Bool
prop_errorThreshold_zero_int i h (Se seed) = not (fst (errorThreshold b i 0 (mkStdGen seed)))
  where
    b = fromList h i

prop_errorThreshold_one_int :: [Int] -> [Hash Int] -> Seed -> Bool
prop_errorThreshold_one_int i h (Se seed) = fst (errorThreshold b i 1 (mkStdGen seed))
  where
    b = fromList h i

prop_contains_consistent_bin :: (CustomMap MinBinaryNum) => MinBinaryNum -> [Hash Int] -> Bool
prop_contains_consistent_bin ibin hashes = exists i b'
  where
    b = create hashes
    b' = insert i b
    i = convert ibin

prop_toList_consistent_bin :: (CustomMap MinBinaryNum) => [MinBinaryNum] -> [Hash Int] -> Bool
prop_toList_consistent_bin ibin hashes = all (`exists` b) i
  where
    b = fromList hashes i
    i = fmap convert ibin

prop_errorThreshold_zero_bin :: (CustomMap MinBinaryNum) => [MinBinaryNum] -> [Hash Int] -> Seed -> Bool
prop_errorThreshold_zero_bin is h (Se seed) = not (fst (errorThreshold b i 0 (mkStdGen seed)))
  where
    b = fromList h i
    i = fmap convert is

prop_errorThreshold_one_bin :: (CustomMap MinBinaryNum) => [MinBinaryNum] -> [Hash Int] -> Seed -> Bool
prop_errorThreshold_one_bin is h (Se seed) = fst (errorThreshold b i 1 (mkStdGen seed))
  where
    b = fromList h i
    i = fmap convert is

prop_calibrate_consistent_threshold_int :: [Int] -> Int -> Double -> Seed -> Bool
prop_calibrate_consistent_threshold_int list num trs (Se seed) =
  fst (errorThreshold b list trs gen)
  where
    (b, m) = calibrateHashFunctions list num trs gen
    gen = mkStdGen seed

-- helper function for convenient conversion from integer representation of binary
-- to custom datatype for testing
binary :: Int -> MinBinaryNum
binary = undefined

-- not sure if the one for the "bad binary" will terminate or not so no quickcheck there
{-
tBinary :: Test
tBinary =
  "binary"
    ~: TestList
      [ zip "abc" [True, False, True] ~?= [('a', True), ('b', False), ('c', True)],
        zip "abc" [True] ~?= [('a', True)],
        zip [] [] ~?= ([] :: [(Int, Int)])
      ]

z :: (CustomMap a) => [a] -> (BloomFilter Int, StdGen)
z m = calibrateHashFunctions (fmap convert m) 100 0.1 (mkStdGen 1)
-}