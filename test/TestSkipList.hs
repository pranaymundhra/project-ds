module TestSkipList where

import HashFunction
import SkipList
import Test.HUnit
import Data.List (nub, sort)
import Test.QuickCheck

prop_length_fromList :: [Int] -> Property
prop_length_fromList list = forAll slGen $ \sl ->
  SkipList.length sl == Prelude.length uniques
  where
    uniques = nub (sort list)
    slGen = fromList uniques

prop_fromList_toList_inverses :: [Int] -> Property
prop_fromList_toList_inverses list = forAll slGen $ \sl ->
  SkipList.toList sl == uniques
  where
    uniques = nub (sort list)
    slGen = fromList uniques

prop_skipList_insertion :: SkipList Int -> Int -> Bool
prop_skipList_insertion = undefined

prop_skipList_deletion :: SkipList Int -> Bool
prop_skipList_deletion = undefined
