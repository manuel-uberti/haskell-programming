{-# LANGUAGE FlexibleInstances #-}

module Chapter16.Exercises where

import GHC.Arr
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
functorCompose f g x = fmap g (fmap f x) == fmap (g . f) x

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

-- Determine if a valid Functor can be written for the datatype provided.
-- 1
-- Bool has kind *, not * -> *, so there can be no instance of Functor

-- 2
data BoolAndSomethingElse a
  = False' a
  | True' a

instance Functor BoolAndSomethingElse where
  fmap f (False' a) = False' (f a)
  fmap f (True' a) = True' (f a)

-- 3
data BoolAndMaybeSomethingElse a
  = Falsish
  | Truish a

instance Functor BoolAndMaybeSomethingElse where
  fmap _ Falsish = Falsish
  fmap f (Truish a) = Truish (f a)

-- 4
newtype Mu f = InF
  { outF :: f (Mu f)
  }

-- Mu has kind (* -> *) -> *, which is not * -> * as we need for the Functor
-- instance
-- 5
data D =
  D (Array Word Word)
    Int
    Int

-- D has kind *, which is not * -> * as we need for the Functor instance
-- Rearrange the arguments to the type constructor of the datatype so the
-- Functor instance works.
-- 1
data Sum' a b
  = First' b
  | Second' a

instance Functor (Sum' e) where
  fmap f (First' a) = First' (f a)
  fmap f (Second' b) = Second' b

-- 2
data Company a b c
  = DeepBlue a
             b
  | Something c

instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

-- 3
data More a b
  = L b
      a
      b
  | R a
      b
      a
  deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

-- Write Functor instances for the following datatypes.
-- 1
data Quant a b
  = Finance
  | Desk a
  | Bloor b

instance Functor (Quant e) where
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Bloor b) = Bloor (f b)

-- 2
data K a b =
  K a

instance Functor (K e) where
  fmap _ (K a) = K a

-- 3
newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

instance Functor (Flip K a) where
  fmap f (Flip (K a)) = Flip $ K (f a)

-- 4
data EvilGoateeConst a b =
  GoatyConst b

instance Functor (EvilGoateeConst e) where
  fmap f (GoatyConst a) = GoatyConst (f a)

-- 5
data LiftItOut f a =
  LiftItOut (f a)

instance Functor e => Functor (LiftItOut e) where
  fmap f (LiftItOut a) = LiftItOut (fmap f a)

-- 6
data Parappa f g a =
  DaWrappa (f a)
           (g a)

instance (Functor e, Functor f) => Functor (Parappa e f) where
  fmap f (DaWrappa a b) = DaWrappa (fmap f a) (fmap f b)

-- 7
data IgnoreOne f g a b =
  IgnoringSomething (f a)
                    (g b)

instance Functor g => Functor (IgnoreOne f g h) where
  fmap f (IgnoringSomething a b) = IgnoringSomething a (fmap f b)

-- 8
data Notorious g o a t =
  Notorious (g o)
            (g a)
            (g t)

instance Functor a => Functor (Notorious a b c) where
  fmap f (Notorious a b c) = Notorious a b (fmap f c)

-- 9
data List a
  = Nil
  | Cons a
         (List a)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a b) = Cons (f a) (fmap f b)

-- 10
data GoatLord a
  = NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a)
              (GoatLord a)
              (GoatLord a)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats a b c) = MoreGoats (fmap f a) (fmap f b) (fmap f c)

-- 11
data TalkToMe a
  = Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print a b) = Print a (f b)
  fmap f (Read g) = Read (f . g)
