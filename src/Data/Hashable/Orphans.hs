{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Hashable.Orphans where

import Data.Foldable (foldl')
import Data.Hashable (Hashable(..))
import Data.Sequence (Seq, fromList)
import Test.QuickCheck (quickCheckAll)


-- | A left, strict fold using `hashWithSalt`
instance Hashable a => Hashable (Seq a) where
  hashWithSalt salt = foldl' hashWithSalt salt

-- | Generate a random list, append a random value, and ensure their hashes are different
testHashableSeq :: (Eq a, Hashable a) => a -> [a] -> Bool
testHashableSeq x xs = hash (fromList (x:xs)) /= hash (fromList (  xs))

-- | `quickCheck` `testHashableSeq`
prop_testHashableSeq :: Int -> [Int] -> Bool
prop_testHashableSeq = testHashableSeq

return []
quickCheckHashableSeq :: IO Bool
quickCheckHashableSeq = $quickCheckAll

