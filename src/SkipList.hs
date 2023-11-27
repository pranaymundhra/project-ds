module SkipList
  (
  )
where

import System.Random (StdGen)
import System.Random qualified as Random (mkStdGen, uniform)

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

-- above is used for getting the height- geometrically distributed

data Node a = N {prev :: Maybe (Node a), next :: Maybe (Node a), val :: [a]}

data SkipList a = Slist {height :: Int, layers :: [Node a], gen :: StdGen}

empty :: Maybe Int -> SkipList a
empty Nothing = Slist 0 [] (mkStdGen 1)
empty (Just seed) = Slist 0 [] (mkStdGen seed)

fromList :: Maybe Int -> [a] -> SkipList a
fromList seed = foldr insert (empty seed)

toList :: SkipList a -> [a]
toList (Slist _ l _) = case l of
  [] -> []
  n : ns -> val n

insert :: a -> SkipList a -> SkipList a
insert = undefined

delete :: a -> SkipList a -> SkipList a
delete = undefined

length :: SkipList a -> Int
length (Slist _ l _) = case l of
  [] -> 0
  n : ns -> Prelude.length (val n)

contains :: SkipList a -> a -> Bool
contains = undefined

append :: SkipList a -> SkipList a -> SkipList a
append x y = foldr insert y (toList x)

instance (Eq a) => Eq (SkipList a) where
  (==) :: (Eq a) => SkipList a -> SkipList a -> Bool
  a == b = toList a == toList b
