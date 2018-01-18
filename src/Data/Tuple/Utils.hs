
module Data.Tuple.Utils where


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


