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
  N {next :: Maybe (Node a), val :: a, below :: Maybe (Node a), nHeight :: Int}
  -- placeholder leftmost node for each layer (ensures we search everything accurately)
  | StartNode {next :: Maybe (Node a), below :: Maybe (Node a), nHeight :: Int}

-- no duplicates supported
data SkipList a = Slist {height :: Int, topLeft :: Maybe (Node a)}

-- | Empty skip list
empty :: SkipList a
empty = Slist 0 Nothing

-- | Converts a list of elements into a skip list
fromList :: Ord a => [a] -> Gen (SkipList a)
fromList = foldM (flip insert) empty

-- | Converts a skip list to list
toList :: SkipList a -> [a]
toList (Slist _ tl) = case tl of
  Nothing -> []
  Just n -> listValues (Just $ getBottomLayer n)
    where
      getBottomLayer :: Node a -> Node a
      getBottomLayer sn@(StartNode _ Nothing _) = sn
      getBottomLayer sn@(StartNode _ (Just b) _) = getBottomLayer b
      getBottomLayer n@(N _ _ Nothing _) = n
      getBottomLayer (N _ _ (Just b) _) = getBottomLayer b
      listValues :: Maybe (Node a) -> [a]
      listValues Nothing = []
      listValues (Just (StartNode next _ _)) = listValues next
      listValues (Just (N next v _ _)) = v : listValues next

-- | Inserts the given element into the skip list
insert :: Ord a => a -> SkipList a -> Gen (SkipList a)
insert a (Slist h layers) = do
  insertionHeight <- genGeoVal
  return $ Slist (max insertionHeight h) (insertNode a layers insertionHeight (max insertionHeight h))
    where
      insertNode :: Ord a => a -> Maybe (Node a) -> Int -> Int -> Maybe (Node a)
      insertNode a Nothing iHeight currHeight =
      -- TODO - fix to include placeholder StartNodes on each layer
        if currHeight == 0 then Just $ N Nothing a Nothing 0
        else Just $ N Nothing a (insertNode a Nothing iHeight (currHeight - 1)) currHeight
      -- TODO - general-case insertion logic, make sure to consider case where insertion height > current height
      insertNode a (Just n@(N next val below nHeight)) iHeight currHeight = undefined
      insertNode a _ _ _ = undefined

-- | Deletes the given element from the skip list
delete :: Ord a => a -> SkipList a -> SkipList a
delete a (Slist h Nothing) = Slist 0 Nothing
delete a (Slist h n) = Slist h (searchAndDelete n a)
  where
    searchAndDelete :: Ord a => Maybe (Node a) -> a -> Maybe (Node a)
    searchAndDelete Nothing _ = Nothing
    searchAndDelete (Just (StartNode next below nh)) a =
      case next of
        Nothing -> Just (StartNode next (searchAndDelete below a) nh)
        Just ne@(N nnext nval _ _) ->
          if nval > a then
            Just (StartNode next (searchAndDelete below a) nh)
          else if nval < a then
            Just (StartNode (searchAndDelete next a) below nh)
          else Just (StartNode nnext (searchAndDelete below a) nh)
        _ -> searchAndDelete next a
    searchAndDelete (Just n@(N next val below nh)) a =
      case next of
        Nothing -> Just (N next val (searchAndDelete below a) nh)
        Just ne@(N nnext nval _ _) ->
          if nval > a then
            Just (N next val (searchAndDelete below a) nh)
          else if nval < a then
            Just (N (searchAndDelete next a) val below nh)
          else Just (N nnext val (searchAndDelete below a) nh)
        _ -> searchAndDelete next a

-- Get the length (number of elements) of a skip list
length :: SkipList a -> Int
length (Slist _ tl) = case tl of
  Nothing -> 0
  Just n -> getLength (Just $ getBottomLayer n)
    where
      getBottomLayer :: Node a -> Node a
      getBottomLayer sn@(StartNode _ Nothing _) = sn
      getBottomLayer sn@(StartNode _ (Just b) _) = getBottomLayer b
      getBottomLayer n@(N _ _ Nothing _) = n
      getBottomLayer (N _ _ (Just b) _) = getBottomLayer b
      getLength :: Maybe (Node a) -> Int
      getLength Nothing = 0
      getLength (Just (StartNode next _ _)) = getLength next
      getLength (Just (N next v _ _)) = 1 + getLength next

-- | Checks whether the skip list contains the given element
contains :: Ord a => SkipList a -> a -> Bool
contains (Slist _ Nothing) a = False
contains (Slist _ n) a = search n a
  where
    search :: (Ord a) => Maybe (Node a) -> a -> Bool
    search Nothing _ = False
    search (Just (StartNode next below _)) a =
      case next of
        Nothing -> search below a
        Just ne@(N _ nval _ _) ->
          if nval > a then search below a
          else search next a
        _ -> search next a
    search (Just (N next val below _)) a
      | val == a = True
      | otherwise =
        case next of
          Nothing -> search below a
          Just ne@(N _ nval _ _) ->
            if nval > a then search below a
            else search next a
          _ -> search next a

-- Combine two skip lists
append :: Ord a => SkipList a -> SkipList a -> Gen (SkipList a)
append x y = foldM (flip insert) y (toList x)

-- Pretty-print the skip list layers
prettyPrint :: Show a => SkipList a -> IO ()
prettyPrint (Slist _ Nothing) = putStrLn ""
prettyPrint (Slist h (Just tl)) = do
  printLayers (Just tl) h
  where
    printNodes :: Show a => Maybe (Node a) -> IO ()
    printNodes Nothing = putStrLn ""
    printNodes (Just StartNode {}) = do
      putStr ". "
    printNodes (Just (N next v _ _)) = do
      putStr $ show v ++ " "
      printNodes next
    printLayers :: Show a => Maybe (Node a) -> Int -> IO ()
    printLayers _ 0 = return ()
    printLayers Nothing _ = return ()
    printLayers (Just node) curHeight = do
      putStr "Layer " >> print curHeight >> putStr ": "
      printNodes (next node)
      printLayers (below node) (curHeight - 1)

instance (Eq a) => Eq (SkipList a) where
  (==) :: (Eq a) => SkipList a -> SkipList a -> Bool
  a == b = toList a == toList b

instance (Show a) => Show (SkipList a) where
  show :: (Show a) => SkipList a -> String
  show sl = show (toList sl)
