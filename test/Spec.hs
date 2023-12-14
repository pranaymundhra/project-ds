module Spec where

import BinaryFilter
import BloomFilter
import Test.HUnit
import Test.QuickCheck
import TestBloomFilter
import TestSkipList

main :: IO ()
main = do
  putStrLn "*** Testing Binary BloomFilter ***"
  TestBloomFilter.qcBin
  putStrLn "*** Testing Integer BloomFilter ***"
  TestBloomFilter.qcInt
  putStrLn "*** Testing SkipList ***"
  TestSkipList.qcSkipList
