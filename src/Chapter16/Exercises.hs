module Chapter16.Exercises where

import Test.QuickCheck

-- Be Kind
-- 1
-- f :: a -> a
-- a: *
-- 2
-- f :: a -> b a -> T (b a)
-- b: * -> *
-- T: * -> * -> *
-- 3
-- f :: c a b -> c b a
-- c: * -> *

-- Heavy Lifting
-- 1
a = fmap (+ 1) $ read "[1]" :: [Int]

-- 2
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

-- 3
c = fmap (* 2) (\x -> x - 2)

-- 4
d = fmap ((return '1' ++) . show) (\x -> [x,1 .. 3])

-- 5
e :: IO Integer
e =
  let ioi = readIO "1" :: IO Integer
      changed = fmap (read . ("123" ++) . show) ioi
   in fmap (* 3) changed

-- Instances of Func
functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == fmap (g . f) x

-- 1
newtype Identity a =
  Identity a
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

testIdentityId :: Eq a => [(Identity a)] -> Bool
testIdentityId x = functorIdentity x

type IdentityInt = Identity Int

composeIdentity :: IdentityInt -> Bool
composeIdentity = functorCompose (+ 1) (* 2)

testComposeIdentity = composeIdentity (Identity 1)

-- 2
data Pair a =
  Pair a
       a
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    return (Pair a a)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

testPairId :: Eq a => [(Pair a)] -> Bool
testPairId x = functorIdentity x

type PairInt = Pair Int

composePair :: PairInt -> Bool
composePair = functorCompose (+ 1) (* 2)

testComposePair = composePair (Pair 1 2)

-- 3
data Two a b =
  Two a
      b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

testTwoId :: (Eq a, Eq b) => [(Two a b)] -> Bool
testTwoId x = functorIdentity x

type TwoInt = Two Int Int

composeTwo :: TwoInt -> Bool
composeTwo = functorCompose (+ 1) (* 2)

testComposeTwo = composeTwo (Two 1 2)

-- 4
data Three a b c =
  Three a
        b
        c
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

testThreeId :: (Eq a, Eq b, Eq c) => [(Three a b c)] -> Bool
testThreeId x = functorIdentity x

type ThreeInt = Three Int Int Int

composeThree :: ThreeInt -> Bool
composeThree = functorCompose (+ 1) (* 2)

testComposeThree = composeThree (Three 1 2 3)

-- 5
data Three' a b =
  Three' a
         b
         b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Three' a b b)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

testThreeId' :: (Eq a, Eq b) => [(Three' a b)] -> Bool
testThreeId' x = functorIdentity x

type ThreeInt' = Three' Int Int

composeThree' :: ThreeInt' -> Bool
composeThree' = functorCompose (+ 1) (* 2)

testComposeThree' = composeThree' (Three' 1 2 3)

-- 6
data Four a b c d =
  Four a
       b
       c
       d
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
         Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four a b c d)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

testFourId :: (Eq a, Eq b, Eq c, Eq d) => [(Four a b c d)] -> Bool
testFourId x = functorIdentity x

type FourInt = Four Int Int Int Int

composeFour :: FourInt -> Bool
composeFour = functorCompose (+ 1) (* 2)

testComposeFour = composeFour (Four 1 2 3 4)

-- 7
data Four' a b =
  Four' a
        a
        a
        b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Four' a a a b)

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

testFourId' :: (Eq a, Eq b) => [(Four' a b)] -> Bool
testFourId' x = functorIdentity x

type FourInt' = Four' Int Int

composeFour' :: FourInt' -> Bool
composeFour' = functorCompose (+ 1) (* 2)

testComposeFour' = composeFour' (Four' 1 2 3 4)

-- 8
data Trivial =
  Trivial
-- The kind is *, not * -> *, so there cannot be a Functor instance.

-- Possibly
data Possibly a
  = LolNope
  | Yeppers a
  deriving (Eq, Show)

instance Functor Possibly where
  fmap _ LolNope = LolNope
  fmap f (Yeppers a) = Yeppers (f a)

-- Short Exercise
-- 1
data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

-- 2
-- Q: Why is a Functor instance that applies the function only to First,
--    Either’s Left, impossible?
-- A: Because to have an instance of Functor for Sum we treat a as part of f.
--    Sum a b would be of kind *, but we need kind * -> * for Functor.
