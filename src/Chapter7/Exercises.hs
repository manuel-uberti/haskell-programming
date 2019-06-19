module Exercises where

-- Grab Bag
-- 1
mth x y z = x * y * z

mth1 x y = \z -> x * y * z

mth2 x = \y -> \z -> x * y * z

mth3 = \x -> \y -> \z -> x * y * z

-- 2
-- :t mth 3
-- Num a => a -> a -> a
-- 3
addOneIfOdd n =
  case odd n of
    True -> f n
    False -> n
  where
    f = \n -> n + 1

addFive x y =
  (if x > y
     then y
     else x) +
  5

addFiveAn =
  \x ->
    \y ->
      (if x > y
         then y
         else x) +
      5

mflipAn f = \x -> \y -> f y x

mflip f x y = f y x

-- Variety Pack
-- 1
k :: (a, b) -> a
k (x, y) = x

k1 :: Num a => a
k1 = k ((4 - 1), 10)

k2 :: [Char]
k2 = k ("three", (1 + 2))

k3 :: Num a => a
k3 = k (3, True)

-- 2
f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a, b, c) (d, e, f) = ((a, d), (c, f))

-- Case Practice
-- 1
-- functionC x y = if (x > y) then x else y
functionC x y =
  case x > y of
    True -> x
    False -> y

-- 2
-- ifEvenAdd2 n = if even n then (n+2) else n
ifEvenAdd2 n =
  case even n of
    True -> n + 2
    False -> n

-- 3
nums x =
  case compare x 0 of
    LT -> -1
    EQ -> 0
    GT -> 1

-- Artful Dodgy
-- 1
dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10

oneIsOne = dodgy 1

oneIsTwo = (flip dodgy) 2

-- Guard Duty
-- 3
pal :: Eq a => [a] -> Bool
pal xs
  | xs == reverse xs = True
  | otherwise = False

-- 6
numbers :: Integer -> Integer
numbers x
  | x < 0 = -1
  | x == 0 = 0
  | x > 0 = 1

-- Chapter Excercises
-- 3
myEq :: Ord a => a -> a -> Bool
myEq x y = x == y

-- :t myEq 1 => (Num a, Ord a) => a -> Bool
f1 :: a -> a
f1 x = x

-- :t f1 True => f1 True :: Bool
-- Let's write code
-- 1
tensDigit :: Integral a => a -> (a, a)
tensDigit x = divMod x 10

hunsD :: Integral a => a -> (a, a)
hunsD x = divMod x 100

-- 2
foldBool1 :: a -> a -> Bool -> a
foldBool1 x y z =
  case z == True of
    False -> x
    True -> y

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y z
  | z == True = y
  | z == False = x

-- 3
g :: (a -> b) -> (a, c) -> (b, c)
g f (x, y) = (f x, y)
