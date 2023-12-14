module TestSkipList where

import HashFunction
import SkipList
import Test.HUnit
import Data.List (nub, sort, delete)
import Test.QuickCheck

prop_length_fromList :: [Int] -> Property
prop_length_fromList list = forAll slGen $ \sl ->
  SkipList.length sl == Prelude.length uniques
  where
    uniques = nub (sort list)
    slGen = fromList uniques

prop_fromList_toList :: [Int] -> Property
prop_fromList_toList list = forAll slGen $ \sl ->
  SkipList.toList sl == uniques
  where
    uniques = nub (sort list)
    slGen = fromList uniques

prop_length_toList :: [Int] -> Property
prop_length_toList list = forAll slGen $ \sl ->
  Prelude.length (SkipList.toList sl) == SkipList.length sl
  where
    uniques = nub (sort list)
    slGen = fromList uniques

prop_fromList_removes_duplicates :: [Int] -> Property
prop_fromList_removes_duplicates list = forAll slGen $ \sl ->
  SkipList.toList sl == uniques
  where
    uniques = nub (sort list)
    slGen = fromList (sort list)

prop_skipList_insertion_contains :: [Int] -> Int -> Property
prop_skipList_insertion_contains list i = forAll slGen $ \sl ->
  forAll (SkipList.insert i sl) $ \sl' -> contains sl' i
  where
    slGen = SkipList.fromList list

prop_skipList_deletion_toList :: [Int] -> Int -> Property
prop_skipList_deletion_toList list i = list /= [] && i > head uniques
  ==> forAll slGen $ \sl ->
  SkipList.toList (SkipList.delete i sl) ==
    Data.List.delete i (SkipList.toList sl)
  where
    uniques = nub (sort list)
    slGen = SkipList.fromList uniques

prop_skipList_contains_empty :: Int -> Bool
prop_skipList_contains_empty i = not (contains empty i)

qcSkipList :: IO ()
qcSkipList = do
  putStrLn "length consistent with input list" 
  quickCheck TestSkipList.prop_length_fromList
  putStrLn "toList gives list of same elements (sorted and without duplicates)" 
  quickCheck TestSkipList.prop_fromList_toList
  putStrLn "toList preserves length of skip list" 
  quickCheck TestSkipList.prop_length_toList
  putStrLn "fromList removes duplicates" 
  quickCheck TestSkipList.prop_fromList_removes_duplicates
  putStrLn "contains consistent with insertion" 
  quickCheck TestSkipList.prop_skipList_insertion_contains
  putStrLn "toList consistent with deletion" 
  quickCheck TestSkipList.prop_skipList_deletion_toList
  putStrLn "empty skip list contains no elements" 
  quickCheck TestSkipList.prop_skipList_contains_empty
