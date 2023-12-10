module SkipList where

import Data.List (nub)
import HashFunction (Seed (Se))
import System.Random (StdGen)
import System.Random qualified as Random (mkStdGen, uniform)
import Test.QuickCheck

{-
mkStdGen :: Int -> StdGen
mkStdGen = Random.mkStdGen . (* (3 :: Int) ^ (20 :: Int))

uniformBool :: StdGen -> (Bool, StdGen)
uniformBool = Random.uniform

genGeoVal :: StdGen -> (Int, StdGen)
genGeoVal g = go g 0
  where
    go g hold = if m then (hold, g') else go g' (hold + 1)
      where
        (m, g') = uniformBool g
-}

genGeoVal :: Gen Int
genGeoVal = go 0
  where
    go i = do
      x <- chooseInt (0, 1)
      if x == 1 then return i else go (i + 1)


-- above is used for getting the height- geometrically distributed

-- use gen here too
-- with bind you get it independent for free
-- insert type Gen SkipList

data Node a = N {prev :: Maybe (Node a), next :: Maybe (Node a), val :: a, below :: Maybe (Node a)}

-- no duplicates supported
data SkipList a = Slist {height :: Int, layers :: [[Node a]]}

empty :: SkipList a
empty = Slist 0 []

fromList :: Maybe Int -> [a] -> SkipList a
fromList seed = foldr insert empty

toList :: SkipList a -> [a]
toList (Slist _ l) = case l of
  [] -> []
  n : ns -> fmap val n

insert :: a -> SkipList a -> SkipList a
insert = undefined

delete :: a -> SkipList a -> SkipList a
delete = undefined

length :: SkipList a -> Int
length (Slist _ l) = case l of
  [] -> 0
  n : ns -> Prelude.length (fmap val n)

contains :: SkipList a -> a -> Bool
contains = undefined

append :: SkipList a -> SkipList a -> SkipList a
append x y = foldr insert y (toList x)

instance (Eq a) => Eq (SkipList a) where
  (==) :: (Eq a) => SkipList a -> SkipList a -> Bool
  a == b = toList a == toList b
