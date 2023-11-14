module HashFunction
  ( Hash,
  )
where

import Data.IntMap (IntMap)
import Test.QuickCheck (Gen, chooseInt)

data Hash a = Hasher {maxHashed :: Int, doHash :: a -> Int}

exampleHash :: Hash Int
exampleHash = Hasher 6 (`mod` 7)

-- >>> doHash exampleHash 8
-- 1

-- >>> maxHashed exampleHash
-- 5

genIntHasher :: Gen (Hash Int)
genIntHasher = fmap (\x -> Hasher (x - 1) (`mod` x)) (chooseInt (1, 99))

class HashedDataStructure a where
  hashFunctions :: [Hash a]
  internalDataStructure :: IntMap a
