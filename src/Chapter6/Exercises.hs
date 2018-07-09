module Exercises where

import Data.List as List

-- 1
data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
  (TisAn int) == (TisAn int') = int == int'


-- 2
data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
  (Two int1 int2) == (Two int1' int2') =
    (int1 == int1') && (int2 == int2')


-- 3
data StringOrInt = TisAnInt Int | TisAString String

instance Eq StringOrInt where
  (TisAnInt int) == (TisAnInt int') = int == int'
  (TisAString s) == (TisAString s') = s == s'
  (TisAnInt _) == (TisAString _) = False
  (TisAString _) == (TisAnInt _) = False


-- 4
data Pair a = Pair a a

instance Eq a => Eq (Pair a) where
  (Pair a b) == (Pair a' b') = (a == a') && (b == b')


-- 5
data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (Tuple a b) == (Tuple a' b') = (a == a') && (b == b')


-- 6
data Which a = ThisOne a | ThatOne a

instance Eq a => Eq (Which a) where
  (ThisOne a) == (ThisOne a') = a == a'
  (ThatOne a) == (ThatOne a') = a == a'
  (ThisOne _) == (ThatOne _) = False
  (ThatOne _) == (ThisOne _) = False


-- 7
data EitherOr a b = Hello a | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (Hello a) == (Hello a') = a == a'
  (Goodbye a) == (Goodbye a') = a == a'
  (Hello _) == (Goodbye _) = False
  (Goodbye _) == (Hello _) = False


-- Does it typecheck?

-- 1
data Person = Person Bool deriving Show

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)


-- 2
data Mood = Blah | Woot deriving (Eq, Show)

settleDown x = if x == Woot then Blah else x


-- 4
type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"


-- Given a datatype declaration, what can we do?
data Rocks = Rocks String deriving (Eq, Show)

data Yeah = Yeah Bool deriving (Eq, Show)

data Papu = Papu Rocks Yeah deriving (Eq, Show)

-- 1
-- phew = Papu "chases" True -- Rocks and Yeah missing


-- 2
truth = Papu (Rocks "chomskydoz") (Yeah True) -- typechecks correctly


-- 3
equalityForAll :: Papu -> Papu -> Bool
equalityForAll p p' = p == p' -- typechecks correctly


-- 4
-- comparePapus :: Papu -> Papu -> Bool
-- comparePapus p p' = p > p' -- Ord instance missing


-- Match the types

-- 1
i :: Num a => a
-- i :: a -- expects a typeclass contraint
i = 1


-- 2
f :: Float
-- f :: Num a => a -- needs to extend Fractional
f = 1.0


-- 3
-- g :: Float
g :: Fractional a => a
g = 1.0


-- 4
-- h :: Float
h :: RealFrac a => a
h = 1.0


-- 5
-- freud :: a -> a
freud :: Ord a => a -> a
freud x = x


-- 6
-- freud' :: a -> a
freud' :: Int -> Int
freud' x = x


-- 7
myX = 1 :: Int
-- sigmund :: a -> a -- needs to be constrained
sigmund :: Int -> Int
sigmund x = myX

-- 8
sigmund' :: Int -> Int
-- sigmund' :: Num a => a -> a -- needs to be 'lower' than Num (Int)
sigmund' x = myX


-- 9
-- jung :: Ord a => [a] -> a
jung :: [Int] -> Int
jung xs = head (sort xs)


-- 10
-- young :: [Char] -> Char
young :: Ord a => [a] -> a
young xs = head (sort xs)


-- 11
mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
-- signifier :: Ord a => [a] -> a  -- needs to be 'lower' than Ord (Char)
signifier xs = head (mySort xs)


-- Type-Kwon-Do Two

-- 1
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk x y z = x y == z


-- 2
arith :: Num b => (a -> b) -> Integer -> a -> b
arith x 1 y = x y + 1
