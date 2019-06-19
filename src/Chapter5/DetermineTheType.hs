{-# LANGUAGE NoMonomorphismRestriction #-}

module DetermineTheType where

example = 1

-- Determine the type
-- 1
-- (* 9) 6 :: Num a => a
-- head [(0,"doge"),(1,"kitteh")] :: Num a => (a, [Char])
-- head [(0 :: Integer,"doge"),(1,"kitteh")] :: (Integer, [Char])
-- if False then True else False :: Bool
-- length [1, 2, 3, 4, 5] :: Int
-- (length [1, 2, 3, 4, 5]) > (length "TACOCAT") :: Bool
-- 2
-- x = 5
-- y = x + 5
-- w = y * 10 -- Num a => a
-- 3
-- z y = y * 10
-- 4
-- f = 4 / y
-- 5
-- a = "Julie"
-- b = " <3 "
-- c = "Haskell"
-- g = a ++ b ++ c -- [Char]
-- Does it compile?
-- 1
-- bigNum = (^) 5 $ 10
-- wahoo = bigNum $ 10 --> wrong use of $
-- 2
-- x = print
-- y = print "woohoo!"
-- z = x "hello world" --> OK
-- 3
-- a = (+)
-- b = 5
-- c = (+) 10
-- d = c 200
-- 4
-- a = 12 + b
-- b = 1000 * c
-- c = 1 --> b doesn't compile, must add c
-- Write a type signature
-- 1
functionH :: [a] -> a
functionH (x:_) = x

-- 2
functionC :: (Ord a) => a -> a -> Bool
functionC x y =
  if (x > y)
    then True
    else False

-- 3
functionS :: (a, b) -> b
functionS (x, y) = y

-- Given a type, write the function
-- 1
i :: a -> a
i a = a

-- 2
c :: a -> b -> a
c a b = a

-- 3
c'' :: b -> a -> b
c'' b a = b

-- 4
c' :: a -> b -> b
c' a b = b

-- 5
r :: [a] -> [a]
-- r l = tail l
r l = l ++ l

-- 6
co :: (b -> c) -> (a -> b) -> a -> c
co f g x = f (g x)

-- 7
a :: (a -> c) -> a -> a
a f a = a

-- 8
a' :: (a -> b) -> a -> b
a' f a = f a
