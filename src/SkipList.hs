module SkipList where

import Data.List (nub, sort)
import HashFunction (Seed (Se))
import System.Random (StdGen)
import System.Random qualified as Random (mkStdGen, uniform)
import Test.QuickCheck
import Control.Monad

import Debug.Trace as Debug

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

lessThanNodeVal :: Ord a => a -> Node a -> Bool
lessThanNodeVal a (ListNode _ val _) = a < val
lessThanNodeVal a (StartNode _ _) = False
lessThanNodeVal a Null = True

greaterThanNodeVal :: Ord a => a -> Node a -> Bool
greaterThanNodeVal a (ListNode _ val _) = a > val
greaterThanNodeVal a (StartNode _ _) = True
greaterThanNodeVal a Null = False

-- no duplicates supported
data SkipList a = Slist {height :: Int, topLeft :: Node a}

-- | Empty skip list
empty :: SkipList a
empty = Slist 0 Null

-- | Converts a list of elements into a skip list
fromList :: Ord a => [a] -> Gen (SkipList a)
fromList = foldM (flip insert) empty

testContains :: Ord a => Gen (SkipList a) -> a -> Gen Bool
testContains g a = do
  sl <- g
  return $ contains sl a


testDelete :: Ord a => a -> Gen (SkipList a) -> Gen (SkipList a)
testDelete a g = do
  sl <- g
  delete' a sl

-- | Converts a skip list to list
toList :: SkipList a -> [a]
toList (Slist _ tl) = listValues (getBottomLayer tl)
  where
    listValues :: Node a -> [a]
    listValues Null = []
    listValues (StartNode next _) = listValues next
    listValues (ListNode next v _) = v : listValues next

-- | Inserts the given element into the skip list
insert :: Ord a => a -> SkipList a -> Gen (SkipList a)
insert a sl@(Slist h tl) = do
  insertionHeight <- genGeoVal
  if contains sl a then return sl
  else return $ trace (show insertionHeight) (Slist (max insertionHeight h) (snd (insertMain a tl insertionHeight)))
  where
    insertMain :: Ord a => a -> Node a -> Int -> (Int, Node a)
    insertMain a n iHeight
      | iHeight == 0 = (iHeight, insertAtBaseLayer a n)
      | otherwise = insertUp a n iHeight
    insertAtBaseLayer :: Ord a => a -> Node a -> Node a
    insertAtBaseLayer a Null = ListNode Null a Null
    insertAtBaseLayer a (StartNode next b) = StartNode (insertAtBaseLayer a next) b
    insertAtBaseLayer a n@(ListNode next v b)
      | a > v = ListNode (insertAtBaseLayer a next) v b
      | a == v = n
      | otherwise = ListNode n a (insertAtBaseLayer a b)
    insertUp :: Ord a => a -> Node a -> Int -> (Int, Node a)
    insertUp a Null iHeight
      | iHeight == 0 = (iHeight, insertAtBaseLayer a Null)
      | otherwise = (0, StartNode (insertAtBaseLayer a Null) Null)
    insertUp a n iHeight
      | iHeight == 0 = (iHeight, insertAtBaseLayer a n)
      | otherwise =
        let (h, newB) = insertMain a (below n) (iHeight - 1)
        in if h == 0
            then (h, n {next = insertAtBaseLayer a (next n)})
            else (h, StartNode (next n) newB)

{-
-- | Deletes the given element from the skip list
delete :: Ord a => a -> SkipList a -> SkipList a
delete a (Slist h Null) = Slist 0 Null
delete a (Slist h n) = Slist h (searchAndDelete n a)
  where
    fixPointersAux :: Node a -> Node a -> Node a
    fixPointersAux Null nodeToDelete = next nodeToDelete
    fixPointersAux node nodeToDelete =
      node {next = fixPointersAux (next node) nodeToDelete}
    fixPointers :: Node a -> Node a -> Node a
    fixPointers Null _ = Null
    fixPointers prev nodeToDelete =
      (fixPointersAux prev nodeToDelete)
        {below = fixPointers (below prev) (below nodeToDelete)}
    searchAndDelete :: Ord a => Node a -> a -> Node a
    searchAndDelete Null _ = Null
    searchAndDelete node@(StartNode n b) a =
      if lessThanNodeVal a n then node {below = searchAndDelete b a}
      else node {next = searchAndDelete n a}
    searchAndDelete node@(ListNode n v b) a
      | equalsNodeVal a n = fixPointers node n
      | lessThanNodeVal a n = node {below = searchAndDelete b a}
      | otherwise = node {next = searchAndDelete n a}
-}
delete' :: Ord a => a -> SkipList a -> Gen (SkipList a)
delete' _ (Slist _ Null) = return $ Slist 0 Null  -- Skip list is empty, nothing to delete

delete' target (Slist h tl) = do
  (_, updatedTop) <- deleteFromLayer target tl h
  return $ Slist h updatedTop

deleteFromLayer :: Ord a => a -> Node a -> Int -> Gen (Bool, Node a)
deleteFromLayer _ Null _ = return (False, Null)  -- Element not found

-- Delete from the current layer
deleteFromLayer target (StartNode next belowNode) h = do
  (deleted, updatedNext) <- deleteFromLayer target next h
  return (deleted, StartNode updatedNext belowNode)

deleteFromLayer target (ListNode nextNode value belowNode) h
  | equalsNodeVal target nextNode = do
    (deleted, updatedBelow) <- deleteFromLayer target belowNode (h - 1)
    return (deleted, updatedBelow)
  | lessThanNodeVal target nextNode = do
    (deleted, updatedNext) <- deleteFromLayer target nextNode h
    return (deleted, ListNode updatedNext value belowNode)
  | otherwise = do
    (deleted, updatedBelow) <- deleteFromLayer target belowNode (h - 1)
    return (deleted, ListNode nextNode value updatedBelow)

-- Get the length (number of elements) of a skip list
length :: SkipList a -> Int
length (Slist _ tl) = getLength (getBottomLayer tl)
  where
    getLength :: Node a -> Int
    getLength Null = 0
    getLength (StartNode next _) = getLength next
    getLength (ListNode next v _) = 1 + getLength next

-- | Checks whether the skip list contains the given element
contains :: Ord a => SkipList a -> a -> Bool
contains (Slist _ Null) a = False
contains (Slist _ n) a = search n a
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

-- Helper for length and toList
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

prop_length_fromList :: [Int] -> Property
prop_length_fromList list = forAll slGen $ \sl ->
  SkipList.length sl == Prelude.length uniques
  where
    uniques = nub (sort list)
    slGen = fromList uniques
