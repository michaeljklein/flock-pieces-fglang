{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}


-- a pusher is guaranteed to push all it can over at once only if f is pointed and props hold.

-- (i think) a copusher .. iff f is copointed and props hold

-- this effectively reduces (f+gf*) to (fgf?), OMG: is this reduction a pushing of compositions of f inside?
--   yeah, what I've done is taken the set of words in the language, partitioned them on their compositions without all of the (return)'s, and given you a view inside that newly partitioned language.
--   Really, really cool. I think this means that the partition interpretation mught be precise.
--   ok, so let's say we have a partition (p), which is a natual transformation from (g a) to some index functor.
--     ahh, since p is an endofunctor, we only really need to have `p` be an injection, and can make the index functor `f (g a)`
--     really, we want the maximal (f (g a) -> f (f (g a))) function such that pushing over pushes everything that can be over,
--     we want to partition on (g a) such that we can reconstruct the structure of a connected component of g's in the partition inside of (g a)
--  in other words, we need to ensure that the lines of the connected components do not cross the lines of what parts of `f` can be passed through `g`.
--    first of all, in what sense does `f` form a graph, that we can accurrately describe what parts are adjacent?
--      if `f` is pointed, then those components that cannot pass through are those such that: (push == fmap (fmap return) :: f . g -> f . g . f)
--      so we get a relation `can pass through(To) :: f g (a)`, where push is non-trivial
--      we can also use (return) to specify that everything has passed through. we then can know: has anything passed through on `x` step, has everything passed through, really everything about what has passed through when
--      p  :: g a -> b
--      p' :: f (g a) -> b
--      p' (return x) = p x
--      extractor :: f b -> maybe b
--      p' = extractor . fmap p
--      extractor (p <$> return x) = p x
--      ahh, we can have a natural extractor, \y -> { x | f associates x with y} :: f (g a) -> {g a}
--      so we require that. \y -> forall (x :: g a). f x == y => p x :: f (g a) -> Bool (the property maps all of the elements of the preimage to the same value)
--       and that holds forall (f (g a)) `elem` (f (f (g a)))
--       which also has a most general format of: \y -> { x | f associates x with y && x `elem` y}
--       which I believe is both a valid inclusion and the most general inclusion
--       valid inclusion: (x `elem` return x), x `elem` y => forall f. f x `elem` fmap f y
--         if this is all we need then we get it from (x `elem` return x) and f being a functor [else hi]
--           (all singleton (== return x) preimages for `f` implies f == const | identity?)
--      and this predicate has: fmap pred . partition == fmap (const True) . partition
--      and the partition is preserved by crossing `g`?

-- alright, we want to classify each operation as being idempotent, and find what results we have.
--   we already have that all being idempotent implies that `f` is pointed and the second operation is equivalent to fmapping the pointing function up to the difference in types

-- since the category probably doesn't have bool, if it's the unit category then the partition is trivial, else it holds for mapping any pair of elements of C to (False, True)
-- really, it works either way, so we can just skip to it:
--  the checker does (if pred then xx else yy) and then we have: fmap checker . partition == fmap (const xx) . partition
--  question: is the checker function a valid function in the category? I'm not sure.
--  we may have to fall back to a fold.


module Scratch.FGLang where

import Control.Monad (liftM2)
import Data.Hashable.Orphans ()
import Data.Monoid ((<>))
import Data.Sequence hiding (filter)
import GHC.TypeLits (TypeError, ErrorMessage(..), Nat, type (+), type (-))
import Prelude hiding (filter, reverse, map)
import qualified Data.HashSet as S (map, fromList, unions, toList)
import qualified Prelude as P (filter)
import Data.HashSet.Utils (Set, replacements)
import Data.Profunctor (Profunctor)


-- | An isomorphism, as in: https://hackage.haskell.org/package/lens-4.15.4/docs/Control-Lens-Iso.html
type Iso s t a b = forall p f. (Profunctor p, Functor f) => p a (f b) -> p s (f t)

-- | An isomorphism, specialized to two types
type Iso' s a = Iso s s a a


-- | Nest a function on a value some number of times
nest :: Int -> (a -> a) -> a -> a
nest 0 _ x = x
nest n f x = nest (n - 1) f (f x)





-- let choose = let { loop rn rd _ 0 = rn `div` rd; loop _  _  0 _ = 0; loop rn rd n k = loop (rn * n) (rd * k) (n-1) (k-1) } in loop 1 1

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

-- | Three types of join
data JoinTypes = JoinL | JoinR | Joined

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

type family JoinType' (n :: Nat) (a :: *) :: * where
  JoinType' n (f (g (f a))) = JoinType' (1 + n) (f (g a))
  JoinType' n    (f (g a))  = JoinType'' f g n a

type family JoinType'' (f :: * -> *) (g :: * -> *) (n :: Nat) (a :: *) :: * where
  JoinType'' f g  0 a = F f (G g a)
  JoinType'' f g  n a = JoinType'' f g (n - 1) (f a)

class Joining' (JoinType a) (JoinType b) => Joining a b where
  joining :: a -> b

-- | Generic function typeclass, helper for `Joining`
class Joining' a b where
  joining' :: a -> b

instance (Functor f, Joining' (F f a) (F f b)) => Joining' (F f (F f a)) (F f (F f b)) where
  joining' = fmap joining'

-- instance Joining' (F f (G g (F f a)))

-- class JoinedC n m f g a b

-- data Joined n m f g a = forall b. JoinedC n m f g a b. Joined b

-- SomeJoined f g a { Joined n m f g a }

-- Joined n m f g a -> Joined n' m' f g a



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








-- instance


-- A is an object in C.

-- We want: functor isomorphisms between these compositions of F and G:
--   F(G(A))    -- we have a composition of functors
--   F(F(G(A))) -- we partition the outer functor into pieces such that `unique partition (xs :: f (g a))` is true for our partition (if it exists, of course, that's where graph coloring comes in)
--   F(G(F(A))) -- we push the partition into the inner functor, resulting in the connected (f (g a)) pieces being joined along their partitions.
--              -- In other words, it pulls the partition up to F's level while pushing the structure of F down into G. The structure of F is available to G in slices (local by definition of the partition).
--              -- finally, all of these operations are invertible since we have unique partitions
--              -- additionally, the functors which this works for are local functors and computers _really_ like computational and data locality. like they simply adore it.
--              --   computational locality makes stream and massively parallel processing a breeze (case in point, the line-by-line parser I just wrote)

--   f :: X -> Y => F(G(f)) :: F(G(X)) -> F(G(Y))

-- That is, we want three natural transformations that are also isomorphisms between those compositions:
--
-- @
--   X :: C => eta_1(X) :: F(G(X)) -> F(F(G(X))) :: C -> C
--   X :: C => eta_2(X) :: F(F(G(X))) -> F(G(F(X))) :: C -> C
--   X :: C => eta_3(X) :: F(G(F(X))) -> F(G(X)) :: C -> C
--
--   f :: X -> Y :: C -> C => eta_1(Y) . F(G(f)) = F(F(G(f))) . eta_1(X) :: F(G(X)) -> F(F(G(Y))) :: C -> C
--   f :: X -> Y :: C -> C => eta_2(Y) . F(F(G(f))) = F(G(F(f))) . eta_2(X) :: F(F(G(X))) -> F(G(F(Y))) :: C -> C
--   f :: X -> Y :: C -> C => eta_3(Y) . F(G(F(f))) = F(G(f)) . eta_3(X) :: F(G(F(X))) -> F(G(Y)) :: C -> C
-- @
--
-- We require these natural transformations to be bijections.


-- The language seeded by: FG, FFG, FGF with the rules: FG == FFG == FGF
--   is equivalent to the regular language generated by the expression: (F+)G(F?)
--
-- We can use this to provide an isomorphism with a more combinatorically friendly type (Nat, Bool)
--   gen :: Nat -> Bool -> Lang
--   gen n False = replicate n 'F' <> "FG"
--   gen n _    = replicate n 'F' <> "FGF"
--
--   unGen :: Lang -> (Nat, Bool)
--   unGen xs = (length (takeWhile (== 'F') xs) - 1, last xs == 'F')
--
-- We can also redefine the language as:
--   base term:
--     FG
--   replacement rules:
--     s/G$/GF/   --\____ toggle ending F
--     s/F$//     --/
--     s/^FF/F/   --\____ can always prepend an F, can remove an F unless it matches /^FG/
--     s/^/F/     --/



-- the lines are natural isomorphisms and the '.' is functor composition
--
--             (f . g)
--               / \
--              /   \
--             /     \
--            /       \
--           /         \
--   f . (f . g) --- (f . g) . f
--
-- I really, really like this diagram.
-- It completely explains the structure of this language


--
-- h = f . h
-- h -> f . h
-- h . g -> h . g . f
--
-- f . h -> h
-- h . g . f -> h . g
--




-- | Just `Bool`, tried making it its own data type
--
-- @
--   f . g     = f . f . g
--   f . f . g = f . g . f
--   f . g     = f . g . f
--
--   (f . g) . f = (f . g . f) . f
-- @
--
-- @
-- data Lang = F | G deriving (Eq, Ord, Show)
--
-- FG = FFG = FGF
--
-- instance Hashable Lang where
--   hashWithSalt = hashUsing (== F)
-- @
--
type Lang = Bool

-- | An expression is a sqeuence of `Lang`s
type Expr  = Seq Lang


parse :: String -> Expr
parse = fromList . fmap (\x -> if x == 'F' then False else True) . P.filter (liftM2 (||) (== 'F') (== 'G'))

-- | Abbreviation for `parse`
p :: String -> Expr
p = parse




-- | First base
--
-- @
-- [F, G]
-- @
--
base1 :: Expr
base1 = [True, False]

-- | Second base
--
-- @
-- [F, F, G]
-- @
--
base2 :: Expr
base2 = [True, False, False]

-- | Third base
--
-- @
-- [F, G, F]
-- @
--
base3 :: Expr
base3 = [True, False, True]

-- | `base1`, `base2`, and `base3`
--
bases :: Set Expr
bases = S.fromList [base1, base2, base3]

-- | All replacements of an expression
allReplacements :: Expr -> Set Expr
allReplacements = (\x y z -> x <> y <> z) <$> replacements base1 [base2, base3] <*> replacements base2 [base1, base3] <*> replacements base3 [base1, base2]

-- | Alias for `allReplacements`
--
-- See below for example printing function implementation (not working):
--
-- @
-- s :: Set Expr -> IO ()
-- s x = (mapM_ (\y _) -> putStrLn . fmap (\z -> if z then 'F' else 'G') . (toList :: Expr -> [Lang]) $ y) . reverse . sort . fromList . S.toList) x >> putStrLn []
-- @
--
r0 :: Expr -> Set Expr
r0 = allReplacements

-- | Iterate on a `Set` of expressions with `allReplacements`
rs :: Set Expr -> Set Expr
rs = S.unions . S.toList . S.map allReplacements

-- | Iterate as in `rs`, but keep already seen values
ex :: Set Expr -> Set Expr
ex x = rs x <> x


