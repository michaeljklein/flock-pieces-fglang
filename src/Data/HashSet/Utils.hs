{-# LANGUAGE TemplateHaskell #-}

module Data.HashSet.Utils where

import Data.Bifunctor (Bifunctor(first))
import Data.HashSet hiding (toList, foldl', singleton, fromList)
import Data.Hashable (Hashable(..))
import Data.Hashable.Orphans ()
import Data.List (nub)
import Data.Monoid ((<>))
import Data.Sequence hiding (filter, length)
import Data.Tuple.Utils (first3, fst3, snd3)
import Test.QuickCheck (quickCheckAll)
import qualified Data.HashSet as S (map, unions, filter, toList)


-- | Convenience synonym
type Set = HashSet

-- | All splits of a sequence @(initial, current, rest)@
allSplits :: (Eq a, Hashable a) => Seq a -> Set (Seq a, a, Seq a)
allSplits xs = case viewl xs of
                 EmptyL     -> mempty
                 ~(y :< ys) -> insert (mempty, y, ys) $ S.map (first3 (y <|)) (allSplits ys)

-- @length seq == length set@
prop_allSplitsLength :: [Int] -> Bool
prop_allSplitsLength xs = length (S.toList (allSplits (fromList xs))) == length xs

-- | Each should be equal to the original, when put back together
prop_allSplitsAreSplits :: [Int] -> Bool
prop_allSplitsAreSplits xs@([]) = (== []) . S.toList . allSplits . fromList $ xs
prop_allSplitsAreSplits xs      = (== [xs']) . S.toList . S.map (\(ys,z,ws) -> ys <> fromList [z] <> ws) . allSplits $ xs'
  where
    xs' = fromList xs

-- The lengths should be @[0..]@
prop_allSplitsHaveExpectedLengths :: [Int] -> Bool
prop_allSplitsHaveExpectedLengths xs = (S.toList . S.map (length . fst3) . allSplits . fromList) xs == [0..length (nub xs)-1]


-- | All splits of a sequence @(initial, rest)@
allSplits_ :: (Eq a, Hashable a) => Seq a -> Set (Seq a, Seq a)
allSplits_ xs = case viewl xs of
                 EmptyL     -> mempty
                 ~(y :< ys) -> insert (singleton y, ys) $ S.map (first (y <|)) (allSplits_ ys)

-- | @length seq == length set@
prop_allSplits_Length :: [Int] -> Bool
prop_allSplits_Length xs = length (S.toList (allSplits_ (fromList xs))) == length xs

-- | Each should be equal to the original, when put back together
prop_allSplits_AreSplits :: [Int] -> Bool
prop_allSplits_AreSplits xs@([]) = (== []) . S.toList . allSplits . fromList $ xs
prop_allSplits_AreSplits xs      = (== [xs']) . S.toList . S.map (\(ys,zs) -> ys <> zs) . allSplits_ $ xs'
  where
    xs' = fromList xs

-- The lengths should be @[0..]@
prop_allSplits_HaveExpectedLengths :: [Int] -> Bool
prop_allSplits_HaveExpectedLengths xs = (S.toList . S.map (length . fst) . allSplits_ . fromList) xs == [0..length (nub xs)-1]


-- | Convert to list and then flatten using the union of two sets
flattenSet :: (Eq a, Hashable a) => Set (Set a) -> Set a
flattenSet = S.unions . S.toList

-- | All splits of a sequence on another sequence
splitsOn :: (Eq a, Hashable a) => Seq a -> Seq a -> Set (Seq a, Seq a)
splitsOn sub xs = case viewl sub of
                    EmptyL          -> allSplits_ xs
                    ~(subx :< suby) -> flattenSet . S.map (\(x, _, z) -> S.map (first (x <>)) (splitsOn suby z)) . S.filter ((== subx) . snd3) $ allSplits xs


-- | All one-application replacements
replacements :: (Eq a, Hashable a) => Seq a -> Set (Seq a) -> Seq a -> Set (Seq a)
replacements sub repls = flattenSet . S.map (\(x, y) -> S.map (\z -> x <> y <> z) repls) . splitsOn sub



return []
quickCheckHashSetUtils :: IO Bool
quickCheckHashSetUtils = $quickCheckAll

