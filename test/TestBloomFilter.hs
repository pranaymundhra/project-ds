module TestBloomFilter where

import BinaryFilter
import BloomFilter
import HashFunction
import Test.HUnit
import Test.QuickCheck

instance Arbitrary (Hash Int) where
  arbitrary :: Gen (Hash Int)
  arbitrary = genBoundedIntHasher 5

  shrink :: Hash Int -> [Hash Int]
  shrink h = []

prop_contains_consistent_int :: Int -> [Hash Int] -> Bool
prop_contains_consistent_int i hashes = exists i b'
  where
    b = create hashes
    b' = insert i b

prop_fromList_consistent_int :: [Int] -> [Hash Int] -> Bool
prop_fromList_consistent_int i hashes = all (`exists` b) i
  where
    b = fromList hashes i

prop_errorThreshold_zero_int :: [Int] -> [Hash Int] -> Property
-- you could have the stupid list so this doesn't actually work. Define well later.
prop_errorThreshold_zero_int i h = i /= [] && length i > maxHashed b ==> p (fmap not (errorThreshold' b i 0 (chooseInt (0, maxHashed b))))
  where
    b = fromList h i

p :: Gen Bool -> Property
p gen = forAll gen id

prop_errorThreshold_one_int :: [Int] -> [Hash Int] -> Property
prop_errorThreshold_one_int i h = p (errorThreshold' b i 1 (chooseInt (0, maxHashed b)))
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
    b' = insert i b
    i = convert ibin

prop_toList_consistent_bin :: [MinBinaryNum] -> [Hash Int] -> Bool
prop_toList_consistent_bin ibin hashes = all (`exists` b) i
  where
    b = fromList hashes i
    i = fmap convert ibin

prop_errorThreshold_zero_bin :: [MinBinaryNum] -> [Hash Int] -> Property
prop_errorThreshold_zero_bin is h = i /= [] && length i > maxHashed b ==> p (fmap not (errorThreshold' b i 0 (chooseInt (0, maxHashed b))))
  where
    b = fromList h i
    i = fmap convert is

prop_errorThreshold_one_bin :: [MinBinaryNum] -> [Hash Int] -> Property
prop_errorThreshold_one_bin is h = p (errorThreshold' b i 1 (chooseInt (0, maxHashed b)))
  where
    b = fromList h i
    i = fmap convert is

go :: [Int] -> Int -> Double -> Gen Bool
go list num trs = do
  b <- calibrateHashFunctions' list num trs (genBoundedIntHasher num) (chooseInt (0, num)) (create [])
  errorThreshold' b list trs (chooseInt (0, num))

prop_calibrate_consistent_threshold_int :: [Int] -> Int -> Double -> Property
prop_calibrate_consistent_threshold_int list num trs = p (go list num trs)

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