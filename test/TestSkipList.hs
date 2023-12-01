module TestSkipList where

import HashFunction
import SkipList
import Test.HUnit
import Test.QuickCheck

prop_length_toList :: [Int] -> Seed -> Bool
prop_length_toList list (Se seed) =
  SkipList.length sl == Prelude.length uniques
    && toList sl == uniques
  where
    uniques = nub list
    sl = fromList (Just seed) uniques

prop_skipList_equality :: [Int] -> Seed -> Seed -> Bool
prop_skipList_equality list (Se seed) (Se second) = sl == sl'
  where
    sl = fromList (Just seed) list
    sl' = fromList (Just second) list

prop_skipList_insertion :: SkipList Int -> Int -> Bool
prop_skipList_insertion = undefined

prop_skipList_deletion :: SkipList Int -> Bool
prop_skipList_deletion = undefined
