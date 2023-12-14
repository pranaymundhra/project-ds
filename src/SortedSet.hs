module SortedSet where

import SkipList
import Test.QuickCheck

type SortedSet a = SkipList a

empty :: SortedSet a
empty = SkipList.empty

-- add to the sorted set
add :: Ord a => a -> SortedSet a -> Gen (SortedSet a)
add = SkipList.insert

-- contains
contains :: Ord a => SortedSet a -> a -> Bool
contains = SkipList.contains

-- equals
equals :: Ord a => SortedSet a -> SortedSet a -> Bool
equals x y = x == y

-- isEmpty
isEmpty :: Ord a => SortedSet a -> Bool
isEmpty s = SkipList.length s == 0

-- remove the element from the sorted set
remove :: Ord a => a -> SortedSet a -> Gen (SortedSet a)
remove = SkipList.delete'

-- size
size :: Ord a => SortedSet a -> Int
size = SkipList.length

-- toSortedList
toSortedList :: Ord a => SortedSet a -> [a]
toSortedList = SkipList.toList

-- fromList
fromList :: Ord a => [a] -> Gen (SortedSet a)
fromList = SkipList.fromList

-- get the first element of the sorted set
first :: SortedSet a -> Maybe a
first (Slist _ tl) = firstVal (SkipList.getBottomLayer tl)
  where
    firstVal :: Node a -> Maybe a
    firstVal Null = Nothing
    firstVal (StartNode next _) = firstVal next
    firstVal (ListNode next v _) = Just v
