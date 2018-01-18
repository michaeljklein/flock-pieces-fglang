{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Data.Functor.Turn where


-- | A `Functor` `Turn`ing class..
--
-- Ideas for implementing this language as a Haskell class:
--
-- @
--  givenExpression :: f (g a)
--  rturn ::        any (g a) <-> any (g (f a))
--  rturn :: C h => h   (g a) <-> h   (g (f a))
--
--  lturn ::        any a <-> f (any a)
--  lturn :: C h => h   a  -> f (h   a)
-- @
--
-- @
-- class C f g where
--   base0 :: Turn f g (Compose f g) => ()
--   base1 :: Turn f g (Compose f (Compose g f)) => ()
--
--   lturnWitness :: (Turn f g h => ()) -> (Turn f g (Compose f h) => ())
--
-- instance C (Base f g)
-- instance C (Compose h g) => C (Compose h (Compose g f))
-- instance C h => C (Compose f h)
-- @
--
class Turn f g h | h -> f, h -> g where
  -- | Turn left
  lturn  :: h a -> f (h a)

  -- | Turn right
  rturn  :: h (g a) -> h (g (f a))

-- | categorical dual of Turn
class Coturn f g h | h -> f, h -> g where
  -- | Co-Turn left
  clturn :: f (h a) -> h a

  -- | Co-Turn right
  crturn :: h (g (f a)) -> h (g a)


-- | The free implementation of a "freely turning" data type
data FreeTurn f g a where
  FreeLTurn :: f (FreeTurn f g a) -> FreeTurn f g a
  FreeRTurn :: FreeTurn f g (g a) -> FreeTurn f g (g (f a))

instance Functor f => Functor (FreeTurn f g) where
  fmap f (FreeLTurn x) = FreeLTurn (fmap f <$> x)
  fmap _ (FreeRTurn _) = error "Absurd: g (f a) ~ a"


-- | Can we prove that @(lturn (FreeRTurn x))@ is a type error?
--
-- @
--  FreeRTurn x :: FreeTurn f g (g (f a))
--  lturn       :: FreeTurn f g       a   -> f (FreeTurn f g a)
--
--  Thus g (f a) ~ a, a type error in standard haskell
-- @
--
-- Q.E.D.
--
instance Turn f g (FreeTurn f g) where
  lturn :: FreeTurn f g a -> f (FreeTurn f g a)
  lturn (FreeLTurn x) = x
  lturn (_          ) = error "Absurd: g (f a) ~ a"

  rturn :: FreeTurn f g (g a) -> FreeTurn f g (g (f a))
  rturn = FreeRTurn


-- | Free `Coturn`
data FreeCoturn f g a where
  FreeLcoturn :: f (FreeCoturn f g a) -> FreeCoturn f g a
  FreeRcoturn :: FreeCoturn f g (g (f a)) -> FreeCoturn f g (g a)

instance Functor f => Functor (FreeCoturn f g) where
  fmap f (FreeLcoturn x) = FreeLcoturn (fmap f <$> x)
  fmap _ (FreeRcoturn _) = error "Absurd: g (f a) ~ a"

-- | This one is easy
instance Coturn f g (FreeCoturn f g) where
  clturn :: f (FreeCoturn f g a) -> FreeCoturn f g a
  clturn = FreeLcoturn

  crturn :: FreeCoturn f g (g (f a)) -> FreeCoturn f g (g a)
  crturn = FreeRcoturn

-- | Left and right turning data structure
data Turning f g a where
  LTurn   :: f (Turning f g a)     -> Turning f g a
  RTurn   :: Turning f g (g a)     -> Turning f g (g (f a))
  RCoturn :: Turning f g (g (f a)) -> Turning f g (g a)

instance Functor f => Functor (Turning f g) where
  fmap f (LTurn x) = LTurn (fmap f <$> x)
  fmap _ _ = error "Absurd: a ~ f a"

instance Turn f g (Turning f g) where
  lturn :: Turning f g a -> f (Turning f g a)
  lturn (LTurn x) = x
  lturn (      _) = error "Absurd: f (g a) ~ a"

  rturn :: Turning f g (g a) -> Turning f g (g (f a))
  rturn = RTurn

instance Coturn f g (Turning f g) where
  clturn :: f (Turning f g a)     -> Turning f g a
  clturn = LTurn

  crturn :: Turning f g (g (f a)) -> Turning f g (g a)
  crturn = RCoturn


