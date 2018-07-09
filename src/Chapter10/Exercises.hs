module Chapter10.Exercises where

import Data.Time
import Data.Maybe
import Chapter9.Exercises

data DatabaseItem
  = DbString String
  | DbNumber Integer
  | DbDate UTCTime
  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbNumber 1999
  , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  ]

-- 1
isDbDate :: DatabaseItem -> Bool
isDbDate (DbDate _) = True
isDbDate _ = False

getTime :: DatabaseItem -> UTCTime
getTime (DbDate i) = i

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate db = map getTime (filter isDbDate db)

isDbNumber :: DatabaseItem -> Bool
isDbNumber (DbNumber _) = True
isDbNumber _ = False

getNumber :: DatabaseItem -> Integer
getNumber (DbNumber i) = i

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber db = map getNumber (filter isDbNumber db)

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent db = (fromJust . myMaximum) (filterDbDate db)

sumDb :: [DatabaseItem] -> Integer
sumDb db = foldr (+) 0 (filterDbNumber db)

avgDb :: [DatabaseItem] -> Double
avgDb db = fromIntegral s / fromIntegral l
  where
    s = sumDb db
    l = length (filterDbNumber db)


-- Warm up and review

-- 1
stops = "pbtdkg"
vowels = "aeiou"

f :: [(Char, Char, Char)]
f = [(x, y, z) | x <- stops, y <- vowels, z <- stops, x == 'p']

nouns = ["Manuel", "Giulia", "Maggie", "Prince"]
verbs = ["loves", "hugs", "slaps", "plays with"]

g :: [(String, String, String)]
g = [(x, y, z) | x <- nouns, y <- verbs, z <- nouns, x /= z]

-- 2/3
seekritFunc :: (Fractional a) => String -> a
seekritFunc x = s / l
  where
    ws = words x
    s = fromIntegral (sum (map length ws))
    l = fromIntegral (length ws)


-- Rewriting functions using folds

myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) =
  if x == False
    then False
    else myAnd xs

myAnd' :: [Bool] -> Bool
myAnd' [] = True
myAnd' (x:xs) = x && myAnd' xs

myAnd'' :: [Bool] -> Bool
myAnd'' =
  foldr
    (\a b ->
       if a == False
         then False
         else b)
    True

myAnd''' :: [Bool] -> Bool
myAnd''' = foldr (&&) True

-- 1
myOr_ :: [Bool] -> Bool
myOr_ [] = False
myOr_ (x:xs) =
  if x == True
    then True
    else myOr_ xs

myOr' :: [Bool] -> Bool
myOr' [] = False
myOr' (x:xs) = x || myOr' xs

myOr'' :: [Bool] -> Bool
myOr'' =
  foldr
    (\a b ->
       if a == True
         then True
         else b)
    False

myOr''' :: [Bool] -> Bool
myOr''' = foldr (||) False

-- 2
myAny_ :: (a -> Bool) -> [a] -> Bool
myAny_ f [] = False
myAny_ f (x:xs) =
  if f x == True
    then True
    else myAny_ f xs

myAny' :: (a -> Bool) -> [a] -> Bool
myAny' f [] = False
myAny' f (x:xs) = f x || myAny' f xs

myAny'' :: (a -> Bool) -> [a] -> Bool
myAny'' f =
  foldr
    (\a b ->
       if f a == True
         then True
         else b)
    False

-- 3
myElem_ :: Eq a => a -> [a] -> Bool
myElem_ x xs = foldr (\a b -> (x == a) || b) False xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' x xs = any (==x) xs

-- 4
myReverse' :: [a] -> [a]
myReverse' xs = foldl (\a b -> b : a) [] xs

-- 5
myMap :: (a -> b) -> [a] -> [b]
myMap f xs = foldr (\a b -> (f a) : b) [] xs

-- 6
myFilter' :: (a -> Bool) -> [a] -> [a]
myFilter' f xs =
  foldr
    (\a b ->
       if f a == True
         then a : b
         else b)
    []
    xs

-- 7
squish' :: [[a]] -> [a]
squish' xs = foldr (\a b -> foldr (\a b -> a : b) b a) [] xs

-- 8
squishMap' :: (a -> [b]) -> [a] -> [b]
squishMap' f xs = foldr (\a b -> (f a) ++ b) [] xs

-- 9
squishAgain' :: [[a]] -> [a]
squishAgain' xs = squishMap' id xs

-- 10
myMaximumBy' :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy' f (x:xs) =
  foldl
    (\a b ->
       if f a b == GT
         then a
         else b)
    x
    xs

-- 11
myMinimumBy' :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy' f (x:xs) =
  foldl
    (\a b ->
       if f a b == LT
         then a
         else b)
    x
    xs
