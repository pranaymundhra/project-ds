module BloomFilter where

-- ask about tries
import Data.IntSet (IntSet, empty, insert, member)
import HashFunction (Hash (Hasher), Seed (Se), customShow, exampleHash, genBoundedIntHasher)
import System.Random (StdGen)
import System.Random qualified as Random (mkStdGen, uniform)
import Test.QuickCheck

mkStdGen :: Int -> StdGen
mkStdGen = Random.mkStdGen . (* (3 :: Int) ^ (20 :: Int))

data BloomFilter a = Filter
  { maxHashed :: Int,
    internalSet :: IntSet,
    hashFunctions :: [Hash a]
  }

create :: [Hash a] -> BloomFilter a
create h = Filter mh empty h
  where
    mh = foldr (\(Hasher maxh _) acc -> max maxh acc) 0 h

-- empty bloomfilter

-- move into test cases after class demo
b = create [exampleHash]

b' = BloomFilter.insert 1 b

b'' = BloomFilter.insert 2 b'

genb = errorThreshold' b' [1] 0.3 (chooseInt (1, 8))

-- >>> exists 8 b'
-- True

-- >>> sample' genb
-- [True,True,True,True,True,True,True,True,True,True,True]

calb = calibrateHashFunctions' [1] 7 0.2 (genBoundedIntHasher 7) (chooseInt (1, 8)) b''

ml = do
  Filter mh i h <- calb
  pure (fmap customShow h)

-- >>> generate ml
-- ["Hasher with maxHashed = 6,y mod i = 3,x mod i = -2","Hasher with maxHashed = 6,y mod i = 0,x mod i = 1"]

u = do
  a <- calb
  errorThreshold' a [1] 0.15 (chooseInt (1, 8))

-- >>> sample' u
-- [True,True,True,True,True,True,True,True,True,True,True]

fromList :: [Hash a] -> [a] -> BloomFilter a
fromList h = foldr BloomFilter.insert b
  where
    b = create h

insert :: a -> BloomFilter a -> BloomFilter a
insert a (Filter mh set hf) =
  foldr
    ( \n (Filter mh' set' hf') ->
        Filter mh' (Data.IntSet.insert n set') hf'
    )
    (Filter mh set hf)
    x
  where
    x = fmap (\(Hasher _ h) -> h a) hf

addHashFunction :: Hash a -> BloomFilter a -> BloomFilter a
addHashFunction (Hasher m h) (Filter mh set hf) = Filter (max m mh) set (Hasher m h : hf)

addHashFunctions :: [Hash a] -> BloomFilter a -> BloomFilter a
addHashFunctions h (Filter mh set hf) = Filter (max mx mh) set (hf ++ h)
  where
    mx = foldr (\(Hasher maxh _) acc -> max maxh acc) 0 h

-- add hash functions to existing bloom filter

exists :: a -> BloomFilter a -> Bool
exists a (Filter mh set hf) = all (`member` set) x
  where
    x = fmap (\(Hasher _ h) -> h a) hf

-- a generalization of StdGen
-- when restricted to Int, we want it to be StdGen

class RandomGen g a where
  next :: g -> Int -> (a, g)

-- the second param is an optional "bound" needed for integer based hashing

instance RandomGen StdGen Int where
  next :: StdGen -> Int -> (Int, StdGen)
  next s b = Random.uniform s

errorThreshold :: (RandomGen m a) => BloomFilter a -> [a] -> Double -> m -> (Bool, m)
errorThreshold = undefined

errorThreshold' :: (Eq a) => BloomFilter a -> [a] -> Double -> Gen a -> Gen Bool
errorThreshold' filter list threshold gen = go filter list threshold gen 0 10000
  where
    go :: (Eq a) => BloomFilter a -> [a] -> Double -> Gen a -> Int -> Int -> Gen Bool
    go filter list tr gen num left = case left of
      0 -> pure (num <= floor (tr * 10000))
      x -> do
        n <- gen
        if n `elem` list
          then go filter list tr gen num left
          else
            if exists n filter
              then go filter list tr gen (num + 1) (left - 1)
              else go filter list tr gen num (left - 1)

-- this function takes in a bloom filter, a list of added elements, and
-- an acceptable error threshold for false positives,
-- and checks the probability of a false positive versus that threshold
-- an adversarial construction of randomgen can cause this to enter an infinite loop

calibrateHashFunctions :: (RandomGen m (Hash a)) => [a] -> Int -> Double -> m -> (BloomFilter a, m)
calibrateHashFunctions elems size threshold = undefined

calibrateHashFunctions' :: (Eq a) => [a] -> Int -> Double -> Gen (Hash a) -> Gen a -> BloomFilter a -> Gen (BloomFilter a)
calibrateHashFunctions' elems size tr gen gena blm = do
  a <- vectorOf 10 (errorThreshold' blm elems tr gena)
  if and a
    then pure blm
    else do
      (Hasher m d) <- gen
      if m > size
        then calibrateHashFunctions' elems size tr gen gena blm
        else calibrateHashFunctions' elems size tr gen gena (addHashFunction (Hasher m d) blm)

-- can use gen here instead of this type class
-- return a gen bool, gen bloomfilter using injection with pure

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
