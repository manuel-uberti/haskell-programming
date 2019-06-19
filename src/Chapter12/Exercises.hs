module Chapter12.Exercises where

import Data.Char
import Data.List

-- String processing
-- 1
concatWithSpace :: [String] -> String
concatWithSpace xs = concat $ intersperse " " xs

maybeToString :: Maybe String -> String
maybeToString Nothing = ""
maybeToString (Just s) = s

maybesToString :: [Maybe String] -> [String]
maybesToString xs = map maybeToString xs

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe s = Just s

thesToNothings :: [String] -> [Maybe String]
thesToNothings xs = map notThe xs

nothingToA :: Maybe String -> Maybe String
nothingToA Nothing = Just "a"
nothingToA (Just s) = Just s

nothingsToAs :: [Maybe String] -> [Maybe String]
nothingsToAs xs = map nothingToA xs

replaceThe :: String -> String
replaceThe s =
  concatWithSpace $ maybesToString $ nothingsToAs $ thesToNothings $ words s

-- 2
isVowel :: Char -> Bool
isVowel c
  | c == 'a' = True
  | c == 'e' = True
  | c == 'i' = True
  | c == 'o' = True
  | c == 'u' = True
  | otherwise = False

isConsonant :: Char -> Bool
isConsonant c = not (isVowel c)

startsWithVowel :: String -> Bool
startsWithVowel s =
  if isVowel (head s)
    then True
    else False

isThe :: String -> Bool
isThe "the" = True
isThe _ = False

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel [] = 0
countTheBeforeVowel s = go (words s) 0
  where
    go [] acc = acc
    go ws acc =
      if isThe (head ws)
        then if startsWithVowel (head (tail ws))
               then go (tail ws) (acc + 1)
               else go (tail ws) acc
        else go (tail ws) acc

-- 3
countVowels :: String -> Integer
countVowels s =
  foldr
    (\a b ->
       if isVowel a
         then b + 1
         else b)
    0
    s

-- Validate the word
newtype Word' =
  Word' String
  deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord s =
  if length vowels > length consonants
    then Nothing
    else Just (Word' s)
  where
    vowels = filter isVowel s
    consonants = filter isConsonant s

-- It's only Natural
data Nat
  = Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger x

integerToNat :: Integer -> Maybe Nat
integerToNat x
  | x < 0 = Nothing
  | x == 0 = Just Zero
  | otherwise = Just (go x Zero)
  where
    go x' acc =
      if x' == 0
        then acc
        else go (x' - 1) (Succ acc)

-- Small library for Maybe
-- 1
isJust :: Maybe a -> Bool
isJust (Just a) = True
isJust _ = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

-- 2
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee x _ Nothing = x
mayybee _ f (Just y) = f y

-- 3
fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing = x
fromMaybe _ (Just x) = x

-- 4
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

-- 5
catMaybes :: [Maybe a] -> [a]
catMaybes xs =
  foldr
    (\a b ->
       if isJust a
         then just a : b
         else b)
    []
    xs
  where
    just (Just x) = x

-- 6
containsNothing :: [Maybe a] -> Bool
containsNothing [] = False
containsNothing (x:xs) =
  if isNothing x
    then True
    else containsNothing xs

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Nothing
flipMaybe xs =
  if containsNothing xs
    then Nothing
    else Just (catMaybes xs)

-- Small library for Either
isLeft :: Either a b -> Bool
isLeft (Left x) = True
isLeft (Right _) = False

-- 1
lefts' :: [Either a b] -> [a]
lefts' xs =
  foldr
    (\a b ->
       if isLeft a
         then left' a : b
         else b)
    []
    xs
  where
    left' (Left x) = x

-- 2
isRight :: Either a b -> Bool
isRight (Right x) = True
isRight (Left _) = False

rights' :: [Either a b] -> [b]
rights' xs =
  foldr
    (\a b ->
       if isRight a
         then right' a : b
         else b)
    []
    xs
  where
    right' (Right x) = x

-- 3
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (as, bs)
  where
    as = lefts' xs
    bs = rights' xs

-- 4
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Left _) = Nothing
eitherMaybe' f (Right x) = Just (f x)

-- 5
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f g (Left x) = f x
either' f g (Right x) = g x

right'' :: Either a b -> b
right'' (Right x) = x

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f (Left _) = Nothing
eitherMaybe'' f (Right x) = Just (either' right'' f (Right x))

-- Write your own iterate and unfoldr
-- 1
myIterate :: (a -> a) -> a -> [a]
myIterate f x = [x] ++ myIterate f (f x)

-- 2
justA :: Maybe (a, b) -> a
justA (Just (x, _)) = x

justB :: Maybe (a, b) -> b
justB (Just (_, y)) = y

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x = [justA (f x)] ++ myUnfoldr f (justB (f x))

-- 3
betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\b -> Just (b, f b)) x

-- Finally something other than a list!
data BinaryTree a
  = Leaf
  | Node (BinaryTree a)
         a
         (BinaryTree a)
  deriving (Eq, Show)

-- 1
unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f x =
  case f x of
    Just (w, y, z) -> Node (unfold f w) y (unfold f z)
    Nothing -> Leaf

-- 2
treeBuild :: Integer -> BinaryTree Integer
treeBuild n =
  unfold
    (\x ->
       if x == 0
         then Nothing
         else Just (x - 1, x - 1, x - 1))
    n
