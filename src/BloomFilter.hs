module BloomFilter where

import Data.IntSet (IntSet, empty, insert, member)
import Debug.Trace
import HashFunction (Hash (Hasher), Seed (Se), customShow, exampleHash, genBoundedIntHasher)
import System.Random (StdGen)
import System.Random qualified as Random (mkStdGen, uniform)
import Test.QuickCheck (Gen, chooseInt, vectorOf)

{-mkStdGen :: Int -> StdGen
mkStdGen = Random.mkStdGen . (* (3 :: Int) ^ (20 :: Int))-}

data BloomFilter a = Filter
  { maxHashed :: Int,
    internalSet :: IntSet,
    hashFunctions :: [Hash a]
  }

-- >>> maxHashed (fromList [] [0])
-- 0

create :: [Hash a] -> BloomFilter a
create h = Filter mh empty h
  where
    mh = foldr (\(Hasher maxh _) acc -> max maxh acc) 0 h

-- empty bloomfilter

-- move into test cases after class demo
b = create [exampleHash]

b' = BloomFilter.insert 1 b

b'' = BloomFilter.insert 2 b'

genb = errorThreshold' b'' [1] 0.2 (chooseInt (1, 8))

-- >>> exists 8 b'
-- True

-- >>> sample' genb
-- [False,False,False,False,False,False,False,False,False,False,False]

calb = calibrateHashFunctions' [1] 7 0.2 (genBoundedIntHasher 7) (chooseInt (1, 8)) b''

ml = do
  Filter mh i h <- calb
  pure (fmap customShow h)

-- >>> generate ml
-- ["Hasher with maxHashed = 6,y mod i = 2,x mod i = 4","Hasher with maxHashed = 6,y mod i = 0,x mod i = 1"]

u = do
  a <- calb
  errorThreshold' a [1] 0.2 (chooseInt (1, 8))

-- >>> sample' u
-- [True,True,True,True,True,True,True,True,True,True,True]

fromList :: [Hash a] -> [a] -> BloomFilter a
fromList h = foldr BloomFilter.insert b
  where
    b = create h

-- >>> exists 79 (fromList [] [0])
-- True

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

addHashFunction :: Hash a -> BloomFilter a -> [a] -> BloomFilter a
addHashFunction (Hasher m h) (Filter mh set hf) list = Filter (max m mh) newset (Hasher m h : hf)
  where
    x = fmap h list
    newset = foldr Data.IntSet.insert set x

-- add hash functions to existing bloom filter

exists :: a -> BloomFilter a -> Bool
exists a (Filter mh set hf) = all (`member` set) x
  where
    x = fmap (\(Hasher _ h) -> h a) hf

-- a generalization of StdGen
-- when restricted to Int, we want it to be StdGen

{-class RandomGen g a where
  next :: g -> Int -> (a, g)-}

-- the second param is an optional "bound" needed for integer based hashing

{-instance RandomGen StdGen Int where
  next :: StdGen -> Int -> (Int, StdGen)
  next s b = Random.uniform s-}

{-errorThreshold :: (RandomGen m a) => BloomFilter a -> [a] -> Double -> m -> (Bool, m)
errorThreshold = undefined-}

-- make sure to acknowledge the uniformly distributed assumption and show why we're not messing it up

errorThreshold' :: (Eq a) => BloomFilter a -> [a] -> Double -> Gen a -> Gen Bool
errorThreshold' filter list threshold gen = go filter list threshold gen 0 10000
  where
    go :: (Eq a) => BloomFilter a -> [a] -> Double -> Gen a -> Int -> Int -> Gen Bool
    go filter list tr gen num left =
      case left of
        0 -> trace (show num) pure (num <= floor (tr * 10000))
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

-- calibrateHashFunctions :: (RandomGen m (Hash a)) => [a] -> Int -> Double -> m -> (BloomFilter a, m)
-- calibrateHashFunctions elems size threshold = undefined

calibrateHashFunctions' :: (Eq a) => [a] -> Int -> Double -> Gen (Hash a) -> Gen a -> BloomFilter a -> Gen (BloomFilter a)
calibrateHashFunctions' elems size tr gen gena blm = do
  a <- vectorOf 10 (errorThreshold' blm elems tr gena)
  if and a
    then pure blm
    else do
      (Hasher m d) <- gen
      if m > size
        then calibrateHashFunctions' elems size tr gen gena blm
        else calibrateHashFunctions' elems size tr gen gena (addHashFunction (Hasher m d) blm elems)

-- can use gen here instead of this type class
-- return a gen bool, gen bloomfilter using injection with pure

-- take in a set of elements and size of output bloom filter
-- Then generate an appropriate
-- number of hash functions randomly until our false positive error is below the
-- error threshold
-- it is possible for this to enter an infinite loop

-- SUPPORT FOR INTEGER BASED HASHING

{-
uniformInt :: StdGen -> (Int, StdGen)
uniformInt = Random.uniform

instance RandomGen StdGen (Hash Int) where
  next :: StdGen -> Int -> (Hash Int, StdGen)
  next g b = (Hasher b hashF, g'')
    where
      (x, g') = uniformInt g
      (y, g'') = uniformInt g'
      hashF h = (h * x + y) `mod` b-}

{-
x :: (Bool, StdGen)
x = errorThreshold (create [exampleHash]) [] 0.1 (mkStdGen 1)

y :: (BloomFilter Int, StdGen)
y = calibrateHashFunctions [] 100 0.1 (mkStdGen 1)-}

-- Giving integer support means that instead of having to write some kind of generator for
-- some custom datatype, the user simply needs to define a function (ideally injective)
-- from that datatype into the integers and then we can simply use an integer bloom filter
-- If the function is not injective, false positives are more likely due to collision
-- between the images of the function

class CustomMap a where
  convert :: a -> Int

{-
z :: (CustomMap a) => [a] -> (BloomFilter Int, StdGen)
z m = calibrateHashFunctions (fmap convert m) 100 0.1 (mkStdGen 1)-}
