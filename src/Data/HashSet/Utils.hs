-- {-# LANGUAGE OverloadedLists #-}
-- {-# LANGUAGE KindSignatures #-}
-- {-# LANGUAGE TypeOperators #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE FunctionalDependencies #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE GADTs #-}
-- {-# LANGUAGE InstanceSigs #-}
-- {-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE DataKinds #-}


module Data.HashSet.Utils where

import Data.Bifunctor (Bifunctor(first))
import Data.HashSet hiding (toList, foldl', singleton, fromList)
import Data.Hashable (Hashable(..))
import Data.Hashable.Orphans ()
import Data.Monoid ((<>))
import Data.Sequence hiding (filter)
import qualified Data.HashSet as S (map, unions, filter, toList)

-- Kick these out to Data.Tuple.Utils

-- | Apply a function to the first of a triple
first3 :: (a -> b) -> (a, c, d) -> (b, c, d)
first3 f (x, y, z) = (f x, y, z)

-- | Get the first element of a triple
fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

-- | Get the second element of a triple
snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y

-- | Get the third element of a triple
trd3 :: (a, b, c) -> c
trd3 (_, _, z) = z



-- | Convenience synonym
type Set = HashSet

-- | All splits of a sequence @(initial, current, rest)@
allSplits :: (Eq a, Hashable a) => Seq a -> Set (Seq a, a, Seq a)
allSplits xs = case viewl xs of
                 EmptyL     -> mempty
                 ~(y :< ys) -> insert (mempty, y, ys) $ S.map (first3 (y <|)) (allSplits ys)

-- | All splits of a sequence @(initial, rest)@
allSplits_ :: (Eq a, Hashable a) => Seq a -> Set (Seq a, Seq a)
allSplits_ xs = case viewl xs of
                 EmptyL     -> mempty
                 ~(y :< ys) -> insert (singleton y, ys) $ S.map (first (y <|)) (allSplits_ ys)

-- | All splits of a sequence on another sequence
splitsOn :: (Eq a, Hashable a) => Seq a -> Seq a -> Set (Seq a, Seq a)
splitsOn sub xs = case viewl sub of
                    EmptyL          -> allSplits_ xs
                    ~(subx :< suby) -> S.unions . S.toList . S.map (\(x, _, z) -> S.map (first (x <>)) (splitsOn suby z)) . S.filter ((== subx) . snd3) $ allSplits xs

-- | All one-application replacements
replacements :: (Eq a, Hashable a) => Seq a -> Set (Seq a) -> Seq a -> Set (Seq a)
replacements sub repls xs = S.unions . S.toList . S.map (\(x, y) -> S.map (\z -> x <> y <> z) repls) $ splitsOn sub xs


