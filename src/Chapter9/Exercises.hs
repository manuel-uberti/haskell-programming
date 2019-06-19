module Chapter9.Exercises where

import Data.Bool (bool)
import Data.Char

-- EnumFromTo
eftBool :: Bool -> Bool -> [Bool]
eftBool True _ = []
eftBool _ True = [False, True]

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd LT EQ = [LT, EQ]
eftOrd LT GT = [LT, EQ, GT]
eftOrd _ _ = []

eftInt :: Int -> Int -> [Int]
eftInt x y = go x y []
  where
    go x y acc
      | x > y = []
      | x == y = x : acc
      | otherwise = go x (pred y) (y : acc)

eftChar :: Char -> Char -> [Char]
eftChar x y = go x y []
  where
    go x y acc
      | x > y = []
      | x == y = x : acc
      | otherwise = go x (pred y) (y : acc)

-- The Fearful Symmetry
dropAt :: Char -> [Char] -> [Char]
dropAt x xs = (dropWhile (== x) (dropWhile (/= x) xs))

takeUntil :: Char -> [Char] -> [Char]
takeUntil x xs = takeWhile (/= x) xs

myWords :: [Char] -> [[Char]]
myWords s = go s []
  where
    go s acc
      | takeUntil ' ' s == "" = reverse acc
      | otherwise = go (dropAt ' ' s) ((takeUntil ' ' s) : acc)

firstSen = "Tyger Tyger, burning bright\n"

secondSen = "In the forests of the night\n"

thirdSen = "What immortal hand or eye\n"

fourthSen = "Could frame thy fearful symmetry?"

sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines s = go s []
  where
    go s acc
      | takeUntil '\n' s == "" = reverse acc
      | otherwise = go (dropAt '\n' s) ((takeUntil '\n' s) : acc)

shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]

main :: IO ()
main = print $ "Are they equal? " ++ show (myLines sentences == shouldEqual)

-- Square Cube
mySqr = [x ^ 2 | x <- [1 .. 5]]

myCube = [y ^ 3 | y <- [1 .. 5]]

sqrCube = [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]

sqrCubeLen = length sqrCube

-- More Bottoms
itIsMystery :: String -> [Bool]
itIsMystery xs = map (\x -> elem x "aeiou") xs

-- map (\x -> if x == 3 then (-x) else (x)) [1..10]
-- map (\x -> bool x (-x) (x == 3)) [1..10]
-- Filtering
-- 1
f1 :: Integral a => [a] -> [a]
f1 xs = filter (\x -> (rem x 3 == 0)) xs

-- 2
-- length (f1 [1..30])
-- 3
isArticle :: String -> Bool
isArticle s
  | s == "a" = True
  | s == "an" = True
  | s == "the" = True
  | otherwise = False

myFilter :: String -> [String]
myFilter s = filter (\x -> (isArticle x == False)) (words s)

-- Zipping exercises
-- 1
myZip :: [a] -> [b] -> [(a, b)]
myZip xs [] = []
myZip [] ys = []
myZip xs ys = [(head xs, head ys)] ++ myZip (tail xs) (tail ys)

-- 2
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f xs [] = []
myZipWith f [] ys = []
myZipWith f xs ys = [f (head xs) (head ys)] ++ myZipWith f (tail xs) (tail ys)

-- 3
myZip' :: [a] -> [b] -> [(a, b)]
myZip' xs ys = myZipWith (,) xs ys

-- Data.Char
-- 2
onlyUpper :: String -> String
onlyUpper s = filter isUpper s

-- 3
capitalize :: String -> String
capitalize s = [toUpper (head s)] ++ tail s

-- 4
upperCase :: String -> String
upperCase [] = []
upperCase s = [toUpper (head s)] ++ upperCase (tail s)

-- 5
firstUpper :: String -> Char
firstUpper s = toUpper (head s)

-- 6
firstUpper' :: String -> Char
firstUpper' s = (toUpper . head) s

firstUpper'' :: String -> Char
firstUpper'' = toUpper . head

-- Writing your own standard functions
-- 1
myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) =
  if x == True
    then True
    else myOr xs

-- 2
myAny :: (a -> Bool) -> [a] -> Bool
myAny f [] = False
myAny f (x:xs) =
  if f x == True
    then True
    else myAny f xs

-- 3
myElem :: Eq a => a -> [a] -> Bool
myElem a [] = False
myElem a (x:xs) =
  if a == x
    then True
    else myElem a xs

myElemAny :: Eq a => a -> [a] -> Bool
myElemAny x xs = any (== x) xs

-- 4
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- 5
squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

-- 6
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

-- 7
squishAgain :: [[a]] -> [a]
squishAgain xs = squishMap id xs

-- 8
myMaximumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
myMaximumBy f [] = Nothing
myMaximumBy f (x:xs) = go f xs x
  where
    go f [] y = Just y
    go f (x:xs) y =
      go
        f
        xs
        (if f x y == GT
           then x
           else y)

-- 9
myMinimumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
myMinimumBy f [] = Nothing
myMinimumBy f (x:xs) = go f xs x
  where
    go f [] y = Just y
    go f (x:xs) y =
      go
        f
        xs
        (if f x y == LT
           then x
           else y)

-- 10
myMaximum :: (Ord a) => [a] -> Maybe a
myMaximum xs = myMaximumBy compare xs

myMinimum :: (Ord a) => [a] -> Maybe a
myMinimum xs = myMinimumBy compare xs
