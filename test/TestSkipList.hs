module TestSkipList where

import HashFunction
import SkipList
import Test.HUnit
import Test.QuickCheck

prop_length_fromList :: [Int] -> Bool

prop_length_toList list =
  SkipList.length sl == Prelude.length uniques
  where
    uniques = nub list
    sl = fromList uniques

prop_skipList_fromList_toList_inverses :: [Int] -> Bool
prop_skipList_fromList_toList_inverses list =
  toList sl == uniques
  where
    uniques = nub list
    sl = fromList uniques

prop_skipList_insertion :: SkipList Int -> Int -> Bool
prop_skipList_insertion = undefined

prop_skipList_deletion :: SkipList Int -> Bool
prop_skipList_deletion = undefined
