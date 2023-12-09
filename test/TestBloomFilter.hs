module TestBloomFilter where

import BloomFilter
import HashFunction
import Test.HUnit
import Test.QuickCheck

prop_contains_consistent_int :: Int -> [Hash Int] -> Bool
prop_contains_consistent_int i hashes = exists i b'
  where
    b = create hashes
    b' = insert i b

prop_fromList_consistent_int :: [Int] -> [Hash Int] -> Bool
prop_fromList_consistent_int i hashes = all (`exists` b) i
  where
    b = fromList hashes i

prop_errorThreshold_zero_int :: [Int] -> [Hash Int] -> Bool
-- you could have the stupid list so this doesn't actually work. Define well later.
prop_errorThreshold_zero_int i h = undefined
  where
    b = fromList h i

prop_errorThreshold_one_int :: [Int] -> [Hash Int] -> Seed -> Bool
prop_errorThreshold_one_int i h (Se seed) = fst (errorThreshold b i 1 (mkStdGen seed))
  where
    b = fromList h i

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

prop_errorThreshold_zero_bin :: [MinBinaryNum] -> [Hash Int] -> Seed -> Bool
prop_errorThreshold_zero_bin is h (Se seed) = not (fst (errorThreshold b i 0 (mkStdGen seed)))
  where
    b = fromList h i
    i = fmap convert is

prop_errorThreshold_one_bin :: [MinBinaryNum] -> [Hash Int] -> Seed -> Bool
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