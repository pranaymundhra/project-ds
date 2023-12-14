module SkipList where

import Data.List (nub, sort, delete)
import System.Random (StdGen)
import System.Random qualified as Random (mkStdGen, uniform)
import Test.QuickCheck
import Control.Monad

-- used for generating height of an inserted node (geometrically distributed)
genGeoVal :: Gen Int
genGeoVal = go 0
  where
    go i = do
      x <- chooseInt (0, 1)
      if x == 1 then return i else go (i + 1)

data Node a =
  -- an internal node in a skip list layer
  ListNode {next :: Node a, val :: a, below :: Node a}
  -- placeholder leftmost node for each layer (ensures we search all values)
  | StartNode {next :: Node a, below :: Node a}
  -- absence of node (makes code more readable than using Maybe)
  | Null

{- Helpers to directly compare a value to a node. Because a layer always starts with a
   StartNode and ends with a Null, we say that StartNode < all other nodes/values and
   Null > all other nodes/values.
   Makes the code a bit cleaner
-}
-- checks if value a equals the value in the node
equalsNodeVal :: Eq a => a -> Node a -> Bool
equalsNodeVal a (ListNode _ val _) = a == val
equalsNodeVal _ _ = False

-- checks if value a is less than the value in the node
lessThanNodeVal :: Ord a => a -> Node a -> Bool
lessThanNodeVal a (ListNode _ val _) = a < val
lessThanNodeVal a (StartNode _ _) = False
lessThanNodeVal a Null = True

-- checks if value a is greater than the value in the node
greaterThanNodeVal :: Ord a => a -> Node a -> Bool
greaterThanNodeVal a (ListNode _ val _) = a > val
greaterThanNodeVal a (StartNode _ _) = True
greaterThanNodeVal a Null = False

-- no duplicates supported
data SkipList a = Slist {topLeft :: Node a}

-- | Empty skip list
empty :: SkipList a
empty = Slist Null

-- | Converts a list of elements into a skip list
fromList :: Ord a => [a] -> Gen (SkipList a)
fromList = foldM (flip insert) empty

-- | Converts a skip list to list
toList :: SkipList a -> [a]
toList (Slist tl) = listValues (getBottomLayer tl)
  where
    listValues :: Node a -> [a]
    listValues Null = []
    listValues (StartNode next _) = listValues next
    listValues (ListNode next v _) = v : listValues next

-- | Inserts the given element into the skip list
insert :: Ord a => a -> SkipList a -> Gen (SkipList a)
insert a sl@(Slist tl) = do
  insertionHeight <- genGeoVal
  if contains sl a then return sl
  else return (Slist (snd (insertMain a tl insertionHeight)))
  where
    insertMain :: Ord a => a -> Node a -> Int -> (Int, Node a)
    insertMain a n iHeight
      | iHeight == 0 = (iHeight, insertAtBaseLayer a n)
      | otherwise = insertOtherLayer a n iHeight
    insertAtBaseLayer :: Ord a => a -> Node a -> Node a
    insertAtBaseLayer a Null = ListNode Null a Null
    insertAtBaseLayer a (StartNode next b) = StartNode (insertAtBaseLayer a next) b
    insertAtBaseLayer a n@(ListNode next v b)
      | a > v = ListNode (insertAtBaseLayer a next) v b
      | a == v = n
      | otherwise = ListNode n a (insertAtBaseLayer a b)
    insertOtherLayer :: Ord a => a -> Node a -> Int -> (Int, Node a)
    insertOtherLayer a Null iHeight
      | iHeight == 0 = (iHeight, insertAtBaseLayer a Null)
      | otherwise = (0, StartNode (insertAtBaseLayer a Null) Null)
    insertOtherLayer a n iHeight
      | iHeight == 0 = (iHeight, insertAtBaseLayer a n)
      | otherwise =
        let (h, newB) = insertMain a (below n) (iHeight - 1)
        in if h == 0
            then (h, n {next = insertAtBaseLayer a (next n)})
            else (h, StartNode (next n) newB)

-- | Deletes the given element from the skip list
delete :: Ord a => a -> SkipList a -> SkipList a
delete k (Slist tl) = Slist (searchAndDelete tl)
  where
    searchAndDelete Null = Null
    searchAndDelete node
      | equalsNodeVal k (next node) =
        updatePointers node {next = Null} (next node)
      | greaterThanNodeVal k (next node) =
        node {next = searchAndDelete $ next node}
      | equalsNodeVal k node = updatePointers (StartNode {next = Null, below = below node}) node
      | otherwise = node {below = searchAndDelete $ below node}
    updatePointers Null _ = Null
    updatePointers start toDelete =
      (updatePointersAux start)
      {below = updatePointers (below start) $ below toDelete}
      where
        updatePointersAux Null = next toDelete
        updatePointersAux n = n {next = updatePointersAux $ next n}

-- Get the length (number of elements) of a skip list
length :: SkipList a -> Int
length (Slist tl) = getLength (getBottomLayer tl)
  where
    getLength :: Node a -> Int
    getLength Null = 0
    getLength (StartNode next _) = getLength next
    getLength (ListNode next v _) = 1 + getLength next

-- | Checks whether the skip list contains the given element
contains :: Ord a => SkipList a -> a -> Bool
contains (Slist Null) a = False
contains (Slist n) a = search n a
  where
    search :: (Ord a) => Node a -> a -> Bool
    search Null _ = False
    search (StartNode next below) a =
      if lessThanNodeVal a next then search below a else search next a
    search (ListNode next val below) a = val == a ||
      (if lessThanNodeVal a next then search below a else search next a)

-- Combine two skip lists
append :: Ord a => SkipList a -> SkipList a -> Gen (SkipList a)
append x y = foldM (flip insert) y (toList x)

-- Helper for length and toList - traverses 'below' fields along left spine
getBottomLayer :: Node a -> Node a
getBottomLayer sn@(StartNode _ Null) = sn
getBottomLayer sn@(StartNode _ b) = getBottomLayer b
getBottomLayer n@(ListNode _ _ Null) = n
getBottomLayer (ListNode _ _ b) = getBottomLayer b
getBottomLayer Null = Null

instance (Eq a) => Eq (SkipList a) where
  (==) :: (Eq a) => SkipList a -> SkipList a -> Bool
  a == b = toList a == toList b

instance (Show a) => Show (SkipList a) where
  show :: (Show a) => SkipList a -> String
  show sl = show (toList sl)

instance (Show a) => Show (Node a) where
  show :: (Show a) => Node a -> String
  show Null = "Null"
  show (StartNode n b) = "S."
  show (ListNode n v b) = show v
