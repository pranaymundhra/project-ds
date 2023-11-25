module HashFunction
  ( Hash,
    genBoundedIntHasher,
  )
where

import Control.Monad qualified as Monad
import Data.IntMap (IntMap)
import Test.QuickCheck (Gen, chooseInt, generate)

data Hash a = Hasher {maxHashed :: Int, doHash :: a -> Int}

instance Show (Hash a) where
  show :: Hash a -> String
  show (Hasher maxHashed _) = "Hasher with maxHashed = " ++ show maxHashed

exampleHash :: Hash Int
exampleHash = Hasher 6 (`mod` 7)

-- >>> doHash exampleHash 8
-- 1

-- >>> maxHashed exampleHash
-- 6

genBoundedIntHasher :: Int -> Gen (Hash Int)
genBoundedIntHasher i =
  Monad.liftM2
    (\x y -> Hasher (i - 1) (\h -> mod (h * x + y) i))
    (chooseInt (1, 99))
    (chooseInt (1, 99))

-- >>> generate (genBoundedIntHasher 5)
-- Hasher with maxHashed = 4

customShow :: Hash Int -> String
customShow (Hasher maxHashed doHash) =
  "Hasher with maxHashed = "
    ++ show maxHashed
    ++ ",y mod i = "
    ++ show (doHash 0)
    ++ ",x mod i = "
    ++ show (doHash 1 - doHash 0)

demo :: Int -> Gen String
demo i = do
  x <- genBoundedIntHasher i
  return (customShow x)

-- >>> generate (demo 100000)
-- "Hasher with maxHashed = 99999,y mod i = 7,x mod i = 96"

-- for sufficiently large values input to demo, x and y mod i will simply be x and y.
-- Note that the modulo here will not always be expressed in standard notation for Z/nZ,
-- but will always be congruent to the correct value. No point in writing clunky code for the
-- demo!
