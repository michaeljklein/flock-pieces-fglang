{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}


module Data.Functor.Join where

import Data.Profunctor (Profunctor)
import GHC.TypeLits (TypeError, ErrorMessage(..), Nat, type (+), type (-))


-- | An isomorphism, as in: https://hackage.haskell.org/package/lens-4.15.4/docs/Control-Lens-Iso.html
type Iso s t a b = forall p f. (Profunctor p, Functor f) => p a (f b) -> p s (f t)

-- | An isomorphism, specialized to two types
type Iso' s a = Iso s s a a


-- | Join two functors together
class (Functor f, Functor g) => Join f g where
  partition :: Iso'    (f (g a))  (f (f (g a)))
  pushover  :: Iso' (f (f (g a))) (f (g (f a)))
  push      :: Iso'    (f (g a))  (f (g (f a)))

-- | Join left
type family JointL (a :: *) :: * -> * where
  JointL (f (f (f a))) = JointL (f (f a))
  JointL (f (f (g a))) = f
  JointL    (f (g a))  = f
  JointL  _            = TypeError ('Text "JointL called on something other than a Join composition")

-- | Join right
type family JointR (a :: *) :: * -> * where
  JointR (f (f (f a))) = JointR (f (f a))
  JointR (f (f (g a))) = g
  JointR    (f (g a))  = g
  JointR  _            = TypeError ('Text "JointR called on something other than a Join composition")


-- | Three types of join: left, right, and done
data JoinTypes = JoinL | JoinR | Joined deriving (Eq, Ord, Show)


-- | `undefined`
joinL :: f (f a) -> f a
joinL = undefined

-- | `undefined`
joinR :: Join f g => f (g (f a)) -> f (g a)
joinR = undefined


-- | Simple wrapper
newtype F f a = F { getF :: f a }

instance Functor f => Functor (F f) where
  fmap f (F x) = F (fmap f x)


-- | Simple wrapper
newtype G g a = G { getG :: g a }

instance Functor g => Functor (G g) where
  fmap f (G x) = G (fmap f x)


-- | The type of a join
type family JoinType (a :: *) :: * where
  JoinType (f (f (f a))) = F f (JoinType  (f (f a)))
  JoinType (f (f (g a))) = F f (JoinType  (f (g a)))
  JoinType (f (g (f a))) =      JoinType' 1 (f (g  a))
  JoinType    (f (g a))  = F f (G g a)

-- | If @f (g a))@ then return, else recurse
type family JoinType' (n :: Nat) (a :: *) :: * where
  JoinType' n (f (g (f a))) = JoinType' (1 + n) (f (g a))
  JoinType' n    (f (g a))  = JoinType'' f g n a

-- | If zero, return, else recurse
type family JoinType'' (f :: * -> *) (g :: * -> *) (n :: Nat) (a :: *) :: * where
  JoinType'' f g  0 a = F f (G g a)
  JoinType'' f g  n a = JoinType'' f g (n - 1) (f a)

-- | Next iteration of `Joining`
class Joining' (JoinType a) (JoinType b) => Joining a b where
  joining :: a -> b

-- | Generic function typeclass, helper for `Joining`
class Joining' a b where
  joining' :: a -> b

-- | Notes:
--
-- @
--  instance Joining' (F f (G g (F f a)))
--
--  class JoinedC n m f g a b
--
--  data Joined n m f g a = forall b. JoinedC n m f g a b. Joined b
--
--  SomeJoined f g a { Joined n m f g a }
--
--  Joined n m f g a -> Joined n' m' f g a
-- @
--
instance (Functor f, Joining' (F f a) (F f b)) => Joining' (F f (F f a)) (F f (F f b)) where
  joining' = fmap joining'

-- | Next iteration of `JoinType`
type family JoinType''' (a :: *) :: JoinTypes where
  JoinType''' (f (f (f a))) = 'JoinL --  (f (f a))
  JoinType''' (f (f (g a))) = 'JoinR --  (f (g a))
  JoinType''' (f (g (f a))) = 'JoinL -- (f (g a))
  JoinType'''    (f (g a))  = 'Joined


-- | For this attempt, the instances are still pretty far from compiling:
--
-- @
-- instance Joining'' a b Joined Joined => Joining'' a b JoinL JoinL where
--   joining'' x = joinL . joining'' x . joinR
--
-- instance Joining'' a b Joined Joined => Joining'' a b JoinL JoinR where
--   joining'' x = joinL . joining'' x . joinR
--
-- instance Joining'' a b Joined Joined => Joining'' a b JoinL Joined where
--   joining'' x = joinL . joining'' x
--
-- instance Joining'' a b Joined Joined => Joining'' a b JoinR JoinL where
--   joining'' x = joinR . joining'' x . joinR
--
-- instance Joining'' a b Joined Joined => Joining'' a b JoinR JoinR where
--   joining'' x = joinR . joining'' x . joinR
--
-- instance Joining'' a b Joined Joined => Joining'' a b JoinR Joined where
--   joining'' x = joinR . joining'' x . joinR
--
-- instance Joining'' a b Joined Joined => Joining'' a b Joined JoinL where
--   joining'' x =         joining'' x . joinR
--
-- instance Joining'' a b Joined Joined => Joining'' a b Joined JoinR where
--   joining'' x =         joining'' x . joinR
--
-- instance Joining'' a b Joined Joined => Joining'' a b Joined Joined where
--   joining'' x = id
-- @
--
class (JointL a ~ JointL b, JointR a ~ JointR b, Join (JointL a) (JointR b), JoinType''' a ~ joinTypeA, JoinType''' b ~ joinTypeB) => Joining'' a b (joinTypeA :: JoinTypes) (joinTypeB :: JoinTypes) where
  joining'' :: a -> b



