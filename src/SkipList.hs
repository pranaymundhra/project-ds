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

data Node a = N {
  next :: Maybe (Node a),
  val :: a,
  below :: Maybe (Node a),
  nHeight :: Int
}

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
      getBottomLayer n@(N _ _ Nothing _) = n
      getBottomLayer (N _ _ (Just b) _) = getBottomLayer b
      listValues :: Maybe (Node a) -> [a]
      listValues Nothing = []
      listValues (Just (N next v _ _)) = v : listValues next

-- | Inserts the given element into the skip list
insert :: Ord a => a -> SkipList a -> Gen (SkipList a)
insert a (Slist h layers) = do
  insertionHeight <- genGeoVal
  return $ Slist (max insertionHeight h) (insertNode a layers insertionHeight (max insertionHeight h))
    where
      insertNode :: Ord a => a -> Maybe (Node a) -> Int -> Int -> Maybe (Node a)
      insertNode a Nothing iHeight currHeight =
        if currHeight == 0 then Just $ N Nothing a Nothing 0
        else Just $ N Nothing a (insertNode a Nothing iHeight (currHeight - 1)) currHeight
      insertNode a (Just n@(N next val below nHeight)) iHeight currHeight = undefined

-- | Deletes the given element from the skip list
delete :: Ord a => a -> SkipList a -> SkipList a
delete a (Slist h Nothing) = Slist 0 Nothing
delete a (Slist h n) = Slist h (searchAndDelete n a)
  where
    searchAndDelete :: Ord a => Maybe (Node a) -> a -> Maybe (Node a)
    searchAndDelete Nothing _ = Nothing
    searchAndDelete (Just n@(N next val below nh)) a =
      case next of
        Nothing -> Just (N next val (searchAndDelete below a) nh)
        Just ne@(N nnext nval _ _) ->
          if nval > a then
            Just (N next val (searchAndDelete below a) nh)
          else if nval < a then
            Just (N (searchAndDelete next a) val below nh)
          else Just (N nnext val (searchAndDelete below a) nh)

length :: SkipList a -> Int
length (Slist _ tl) = case tl of
  Nothing -> 0
  Just n -> getLength (Just $ getBottomLayer n)
    where
      getBottomLayer :: Node a -> Node a
      getBottomLayer n@(N _ _ Nothing _) = n
      getBottomLayer (N _ _ (Just b) _) = getBottomLayer b
      getLength :: Maybe (Node a) -> Int
      getLength Nothing = 0
      getLength (Just (N next v _ _)) = 1 + getLength next

-- | Checks whether the skip list contains the given element
contains :: Ord a => SkipList a -> a -> Bool
contains (Slist _ Nothing) a = False
contains (Slist _ n) a = loop n a
  where
    loop :: (Ord a) => Maybe (Node a) -> a -> Bool
    loop Nothing _ = False
    loop (Just (N next val below _)) a
      | val == a = True
      | otherwise =
        case next of
          Nothing -> loop below a
          Just ne@(N _ nval _ _) ->
            if nval > a then loop below a
            else loop next a

append :: Ord a => SkipList a -> SkipList a -> Gen (SkipList a)
append x y = foldM (flip insert) y (toList x)

instance (Eq a) => Eq (SkipList a) where
  (==) :: (Eq a) => SkipList a -> SkipList a -> Bool
  a == b = toList a == toList b

instance (Show a) => Show (SkipList a) where
  show :: (Show a) => SkipList a -> String
  show sl = show (toList sl)