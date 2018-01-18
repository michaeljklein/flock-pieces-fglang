{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Hashable.Orphans () where

import Data.Foldable (foldl')
import Data.Hashable (Hashable(..))
import Data.Sequence (Seq)

-- | A left, strict fold using `hashWithSalt`
instance Hashable a => Hashable (Seq a) where
  hashWithSalt salt = foldl' hashWithSalt salt
