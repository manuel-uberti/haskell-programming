module Exercises where

isPalindrome :: (Eq a) => [ a ] -> Bool
isPalindrome x = x == reverse x

myAbs :: Integer -> Integer
myAbs n = if n > 0 then n else (-n)

f :: ( a, b ) -> ( c, d ) -> ( ( b, d ), ( a, c ) )
f x y = ( e, f )
  where
    e = ( snd x, snd y )

    f = ( fst x, fst y )

x = (+)

g xs = w `x` 1
  where
    w = length xs

myId x = x

h ( a, b ) = a
