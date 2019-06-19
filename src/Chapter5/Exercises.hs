module Exercises where

-- f :: a -> a
-- f a = a + a IMPOSSIBLE
g :: a -> a -> a
g a b = a

-- g a b = b
h :: a -> b -> b
h a b = b

i :: (Num a, Num b) => a -> b -> b
-- i a b = a ERROR
i a b = b

j :: [a] -> Int -> a
j a b = a !! b
