{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Flip where

import Data.Coerce (coerce)
import Data.Functor.Contravariant (Contravariant(..))

import Control.Comonad (Comonad(..))



-- | A four-part mutually recursive data type
--
-- Notes:
--
-- Number of cycles:
--   w.l.o.g we require the first element to be the minimum
--
-- @
-- 1 minimum
-- * permutations of all else
-- (n-1)! ?
-- @
--
-- E.g. for two, there's exactly one cycle:
--
-- @
--   Value, Container =>
--     Value * Container (..), Container (Value * ..)
--     Value + Container (..), Container (Value + ..)
-- @
--
-- Interesting example: unary / binary
--
-- @
-- (a -> b)
-- (a -> b -> c)
--
-- a -> b
-- a -> (b -> c)
-- a -> (a, b, b -> c)
-- b -> (b, c)
-- a -> (b, (b -> (c, next)))
-- @
--
--
-- Want:
--
-- Applicative instances for `F1`, `F3`
--
-- `Comonad` instances for `F2`, `F4`
--
newtype F1 a b c = F1 { getF1 :: a -> F2 a b c  }

-- | Continuation of `F1`
newtype F2 a b c = F2 { getF2 :: (b,  F3 a b c) }

-- | Continuation of `F2`
newtype F3 a b c = F3 { getF3 :: b -> F4 a b c  }

-- | Continuation of `F3`
newtype F4 a b c = F4 { getF4 :: (c,  F1 a b c) }


instance Functor (F1 a b) where
  fmap f (F1 x) = F1 (fmap f <$> x)

instance Functor (F2 a b) where
  fmap f (F2 x) = F2 (fmap f <$> x)

instance Functor (F3 a b) where
  fmap f (F3 x) = F3 (fmap f <$> x)

instance Functor (F4 a b) where
  fmap f (F4 (x, xs)) = F4 (f x, fmap f xs)


-- | Note that of `F1`, `F2`, `F3`, and `F4`, only `F4` is always a `Comonad`:
--
-- `F1` would need `F2` to be a `Comonad`, and a default value for @a@
--
-- `F2` would require `F3` to be a `Comonad`
--
-- `F3` would require a default value for @b@, and for `F4` to be a `Comonad` (which I believe it is)
--
--
-- @
-- extend extract = ext
--   ext (F4 ~(x, xs)) = F4 (x, F1 . fmap (F2 . fmap (F3 . fmap ext . getF3) . getF2) . getF1 $ xs)
--
--   since F1, F2, F3, getF1, getF2, getF3, fmap do not affect the values, aside from @fmap ext@, I believe this may be proof of: ext == id
--
-- extend extract      = id
-- extract . extend f  = f
-- extend f . extend g = extend (f . extend g)
-- @
--
instance Comonad (F4 a b) where
  extract :: F4 a b c -> c
  extract = fst . getF4

  extend :: (F4 a b c -> d) -> F4 a b c -> F4 a b d
  extend f x@(F4 ~(_, xs)) = let y = f x in F4 (y, F1 . fmap (F2 . fmap (F3 . fmap (extend f) . getF3) . getF2) . getF1 $ xs)





-- | Like `flip`
newtype Flip f a b = Flip { getFlip :: f b a }

-- | `coerce`
withFlip :: (Flip f1 a1 b1 -> Flip f a b) -> f1 b1 a1 -> f b a
withFlip = coerce


-- | `Flip` for three arguments
newtype Flip2 f a b c = Flip2 { getFlip2 :: f c b a }

-- | Coerce
withFlip2 :: (Flip2 f1 a1 b1 c1 -> Flip2 f a b c) -> f1 c1 b1 a1 -> f c b a
withFlip2 = coerce


-- | A "functor" that requires an isomorphism to map over
class IsoFunctor f where
  isomap :: (b -> a) -> (a -> b) -> f a -> f b


instance IsoFunctor (Flip (F1 a) c) where
  isomap f g (Flip (F1 x)) = Flip (F1 (withFlip (isomap f g) <$> x))

instance IsoFunctor (Flip (F2 a) c) where
  isomap f g (Flip (F2 (x, xs))) = Flip (F2 (g x, withFlip (isomap f g) xs))

instance IsoFunctor (Flip (F3 a) c) where
  isomap f g (Flip (F3 x)) = Flip (F3 (withFlip (isomap f g) . x . f))

instance IsoFunctor (Flip (F4 a) c) where
  isomap f g (Flip (F4 x)) = Flip (F4 (withFlip (isomap f g) <$> x))



instance Contravariant (Flip2 F1 a b) where
  contramap f (Flip2 (F1 x)) = Flip2 (F1 (withFlip2 (contramap f) . x . f))

instance Contravariant (Flip2 F2 a b) where
  contramap f (Flip2 (F2 x)) = Flip2 (F2 (withFlip2 (contramap f) <$> x))

instance Contravariant (Flip2 F3 a b) where
  contramap f (Flip2 (F3 x)) = Flip2 (F3 (withFlip2 (contramap f) <$> x))

instance Contravariant (Flip2 F4 a b) where
  contramap f (Flip2 (F4 x)) = Flip2 (F4 (withFlip2 (contramap f) <$> x))


