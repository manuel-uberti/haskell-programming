module Chapter17.Examples where

import Control.Applicative

f :: (Eq a, Num a) => a -> Maybe String
f x = lookup x [(3, "hello"), (4, "julie"), (5, "kbai")]

g :: (Eq a, Num a) => a -> Maybe String
g y = lookup y [(7, "sup?"), (8, "chris"), (9, "aloha")]

h :: (Eq a, Num a) => a -> Maybe Int
h z = lookup z [(2, 3), (5, 6), (7, 8)]

m :: (Eq a, Num a) => a -> Maybe Int
m x = lookup x [(4, 10), (8, 13), (1, 9001)]
