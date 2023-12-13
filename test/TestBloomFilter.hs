module TestBloomFilter where

import BinaryFilter
import BloomFilter
import Data.List
import HashFunction
import Test.HUnit
import Test.QuickCheck

instance Arbitrary (Hash Int) where
  arbitrary :: Gen (Hash Int)
  arbitrary = genBoundedIntHasher 5

  shrink :: Hash Int -> [Hash Int]
  shrink h = []

{-newtype Positive = Positive Int

instance Arbitrary (Positive) where
  arbitrary :: Gen (Positive)
  arbitrary = fmap Positive (chooseInt 0, 100)

  shrink :: Positive -> [Positive]
  shrink (Positive x) = fmap Positive (shrink x)-}

prop_contains_consistent_int :: Int -> [Hash Int] -> Bool
prop_contains_consistent_int i hashes = exists i b'
  where
    b = create hashes
    b' = BloomFilter.insert i b

prop_toList_consistent_int :: [Int] -> [Hash Int] -> Bool
prop_toList_consistent_int i hashes = all (`exists` b) i
  where
    b = fromList hashes i

prop_errorThreshold_zero_int :: [Int] -> [Hash Int] -> Property
-- you could have the stupid list so this doesn't actually work. Define well later.
prop_errorThreshold_zero_int i h = i /= [] && not (null h) && length (nub i) > maxHashed b + 1 ==> p (fmap not (errorThreshold' b i 0 (chooseInt (0, maxHashed b))))
  where
    b = fromList h i

-- >>>sample' (errorThreshold' (create []) [0] 0 (chooseInt (0, 0)))

p :: Gen Bool -> Property
p gen = forAll gen id

prop_errorThreshold_one_int :: [Int] -> [Hash Int] -> Property
prop_errorThreshold_one_int i h = not (null h) ==> p (errorThreshold' b i 1 (chooseInt (0, maxHashed b)))
  where
    b = fromList h i

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

prop_toList_consistent_bin :: [MinBinaryNum] -> [Hash Int] -> Bool
prop_toList_consistent_bin ibin hashes = all (`exists` b) i
  where
    b = fromList hashes i
    i = fmap convert ibin

prop_errorThreshold_zero_bin :: [MinBinaryNum] -> [Hash Int] -> Property
prop_errorThreshold_zero_bin is h = i /= [] && not (null h) && length i > maxHashed b + 1 ==> p (fmap not (errorThreshold' b i 0 (chooseInt (0, maxHashed b))))
  where
    b = fromList h i
    i = nub (fmap convert is)

prop_errorThreshold_one_bin :: [MinBinaryNum] -> [Hash Int] -> Property
prop_errorThreshold_one_bin is h = not (null h) ==> p (errorThreshold' b i 1 (chooseInt (0, maxHashed b)))
  where
    b = fromList h i
    i = fmap convert is

go :: [Int] -> Int -> Double -> Hash Int -> Gen Bool
go list num trs h = do
  b <- calibrateHashFunctions' list num trs (genBoundedIntHasher num) (chooseInt (0, num)) (create [h])
  errorThreshold' b list trs (chooseInt (0, num))

prop_calibrate_consistent_threshold_int :: [Int] -> Positive Int -> NonNegative Double -> Hash Int -> Property
prop_calibrate_consistent_threshold_int list (Positive num) (NonNegative trs) h = p (go list num (trs / (trs + 1)) h)

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

qcBin :: IO ()
qcBin = do
  putStrLn "consistent contains"
  quickCheck prop_contains_consistent_bin
  putStrLn "consistent toList"
  quickCheck prop_toList_consistent_bin
  putStrLn "zero Error Threshold"
  quickCheck prop_errorThreshold_zero_bin
  putStrLn "one Error Threshold"
  quickCheck prop_errorThreshold_one_bin

qcInt :: IO ()
qcInt = do
  putStrLn "consistent contains"
  quickCheck prop_contains_consistent_int
  putStrLn "consistent toList"
  quickCheck prop_toList_consistent_int
  putStrLn "zero Error Threshold"
  quickCheck prop_errorThreshold_zero_int
  putStrLn "one Error Threshold"
  quickCheck prop_errorThreshold_one_int
  putStrLn "calibrating consistent hash functions to error threshold"
  quickCheck prop_calibrate_consistent_threshold_int
