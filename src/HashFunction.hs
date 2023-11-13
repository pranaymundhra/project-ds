module HashFunction
  ( Hash,
  )
where

import Test.QuickCheck (Gen, chooseInt)

data Hash a = Hasher {maxHashed :: Int, doHash :: a -> Int}

exampleHash :: Hash Int
exampleHash = Hasher 6 (`mod` 7)

-- >>> doHash exampleHash 8
-- 1

-- >>> maxHashed exampleHash
-- 6

genIntHasher :: Gen (Hash Int)
genIntHasher = fmap (\x -> Hasher (x - 1) (`mod` x)) (chooseInt (0, 99))
