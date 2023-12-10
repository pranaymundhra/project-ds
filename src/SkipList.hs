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

fromList :: [a] -> SkipList a
fromList = foldr insert empty

toList :: SkipList a -> [a]
toList (Slist _ l) = case l of
  [] -> []
  ns : nss -> fmap val ns

insert :: a -> SkipList a -> SkipList a
insert a (Slist _ []) = undefined
insert a (Slist _ ([] : ls)) = undefined
insert a (Slist _ ((n : ns) : ls)) = undefined
    

delete :: a -> SkipList a -> SkipList a
delete = undefined

length :: SkipList a -> Int
length (Slist _ l) = case l of
  [] -> 0
  ns : nss -> Prelude.length (fmap val ns)

contains :: Ord a => SkipList a -> a -> Bool
contains (Slist _ []) a = False
contains (Slist _ ([] : ls)) a = False
contains (Slist _ ((n : ns) : ls)) a = loop (Just n) a
  where
    loop :: Ord a => Maybe (Node a) -> a -> Bool
    loop Nothing _ = False
    loop (Just (N prev next val below)) a =
      val == a || (if val < a then loop next a else
        case prev of 
          Nothing -> False
          Just (N pprev pnext pval pbelow) -> loop pbelow a)

append :: SkipList a -> SkipList a -> SkipList a
append x y = foldr insert y (toList x)

instance (Eq a) => Eq (SkipList a) where
  (==) :: (Eq a) => SkipList a -> SkipList a -> Bool
  a == b = toList a == toList b
