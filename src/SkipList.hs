module SkipList where

import Data.List (nub)
import HashFunction (Seed (Se))
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
  deriving (Eq, Show)

{- Helpers to directly compare a value to a node. Because a layer always starts with a
   StartNode and ends with a Null, we say that StartNode < all other nodes/values and
   Null > all other nodes/values.
   Makes the code a bit cleaner
-}
-- checks if value a equals the value in the node
equalsNode :: Eq a => a -> Node a -> Bool
equalsNode a (ListNode _ val _) = a == val
equalsNode _ _ = False

lessThanNode :: Ord a => a -> Node a -> Bool
lessThanNode a (ListNode _ val _) = a < val
lessThanNode a (StartNode _ _) = False
lessThanNode a Null = True

greaterThanNode :: Ord a => a -> Node a -> Bool
greaterThanNode a (ListNode _ val _) = a > val
greaterThanNode a (StartNode _ _) = True
greaterThanNode a Null = False

-- no duplicates supported
data SkipList a = Slist {height :: Int, topLeft :: Node a}

-- | Empty skip list
empty :: SkipList a
empty = Slist 0 Null

-- | Converts a list of elements into a skip list
fromList :: Ord a => [a] -> Gen (SkipList a)
fromList = foldM (flip insert) empty

-- | Converts a skip list to list
toList :: SkipList a -> [a]
toList (Slist _ tl) = listValues (getBottomLayer tl)
  where
    getBottomLayer :: Node a -> Node a
    getBottomLayer sn@(StartNode _ Null) = sn
    getBottomLayer sn@(StartNode _ b) = getBottomLayer b
    getBottomLayer n@(ListNode _ _ Null) = n
    getBottomLayer (ListNode _ _ b) = getBottomLayer b
    getBottomLayer Null = Null
    listValues :: Node a -> [a]
    listValues Null = []
    listValues (StartNode next _) = listValues next
    listValues (ListNode next v _) = v : listValues next

-- | Inserts the given element into the skip list
insert :: Ord a => a -> SkipList a -> Gen (SkipList a)
insert a (Slist h layers) = do
  insertionHeight <- genGeoVal
  return $ Slist (max insertionHeight h) (insertNode a layers insertionHeight (max insertionHeight h))
    where
      insertNode :: Ord a => a -> Node a -> Int -> Int -> Node a
      insertNode = undefined

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
      (fixPointersAux prev nodeToDelete) {below = fixPointers (below prev) (below nodeToDelete)}
    searchAndDelete :: Ord a => Node a -> a -> Node a
    searchAndDelete Null _ = Null
    searchAndDelete node@(StartNode n b) a =
      if lessThanNode a n then node {below = searchAndDelete b a}
      else node {next = searchAndDelete n a}
    searchAndDelete node@(ListNode n v b) a
      | equalsNode a n = fixPointers node n
      | lessThanNode a n = node {below = searchAndDelete b a}
      | otherwise = node {next = searchAndDelete n a}

-- Get the length (number of elements) of a skip list
length :: SkipList a -> Int
length (Slist _ tl) = getLength (getBottomLayer tl)
  where
    getBottomLayer :: Node a -> Node a
    getBottomLayer sn@(StartNode _ Null) = sn
    getBottomLayer sn@(StartNode _ b) = getBottomLayer b
    getBottomLayer n@(ListNode _ _ Null) = n
    getBottomLayer (ListNode _ _ b) = getBottomLayer b
    getBottomLayer Null = Null
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
      if lessThanNode a next then search below a else search next a
    search (ListNode next val below) a = val == a ||
      (if lessThanNode a next then search below a else search next a)

-- Combine two skip lists
append :: Ord a => SkipList a -> SkipList a -> Gen (SkipList a)
append x y = foldM (flip insert) y (toList x)

-- Pretty-print the skip list layers
prettyPrint :: Show a => SkipList a -> IO ()
prettyPrint (Slist h tl) = do
  printLayers tl h
  where
    printNodes :: Show a => Node a -> IO ()
    printNodes Null = putStrLn ""
    printNodes (StartNode _ _) = do
      putStr ". "
    printNodes (ListNode next v _) = do
      putStr $ show v ++ " "
      printNodes next
    printLayers :: Show a => Node a -> Int -> IO ()
    printLayers _ 0 = return ()
    printLayers Null _ = return ()
    printLayers node h = do
      putStr "Layer " >> print h >> putStr ": "
      printNodes (next node)
      printLayers (below node) (h - 1)

instance (Eq a) => Eq (SkipList a) where
  (==) :: (Eq a) => SkipList a -> SkipList a -> Bool
  a == b = toList a == toList b

instance (Show a) => Show (SkipList a) where
  show :: (Show a) => SkipList a -> String
  show sl = show (toList sl)
