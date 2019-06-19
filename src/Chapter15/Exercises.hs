module Chapter15.Exercises where

import qualified Data.Monoid as Monoid
import Data.Semigroup
import Test.QuickCheck

-- 1
data Trivial =
  Trivial
  deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

-- 2
data Identity a =
  Identity a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity (x <> y)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

type IdentityAssocString
   = Identity String -> Identity String -> Identity String -> Bool

-- 3
data Two a b =
  Two a
      b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two x y) <> (Two x' y') = Two (x <> x') (y <> y')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

type TwoAssoc
   = Two String String -> Two String String -> Two String String -> Bool

-- 4
data Three a b c =
  Three a
        b
        c
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) =>
         Semigroup (Three a b c) where
  (Three x y z) <> (Three x' y' z') = Three (x <> x') (y <> y') (z <> z')

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

type ThreeAssoc
   = Three String String String -> Three String String String -> Three String String String -> Bool

-- 5
data Four a b c d =
  Four a
       b
       c
       d
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) =>
         Semigroup (Four a b c d) where
  (Four w x y z) <> (Four w' x' y' z') =
    Four (w <> w') (x <> x') (y <> y') (z <> z')

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
         Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four a b c d)

type FourAssoc
   = Four String String String String -> Four String String String String -> Four String String String String -> Bool

-- 6
newtype BoolConj =
  BoolConj Bool
  deriving (Eq, Show)

instance Semigroup BoolConj where
  BoolConj True <> BoolConj True = BoolConj True
  BoolConj _ <> BoolConj _ = BoolConj False

instance Monoid BoolConj where
  mempty = (BoolConj True)
  mappend = (<>)

-- 7
newtype BoolDisj =
  BoolDisj Bool
  deriving (Eq, Show)

instance Semigroup BoolDisj where
  BoolDisj False <> BoolDisj False = BoolDisj False
  BoolDisj _ <> BoolDisj _ = BoolDisj True

instance Monoid BoolDisj where
  mempty = (BoolDisj False)
  mappend = (<>)

-- 8
data Or a b
  = Fst a
  | Snd b
  deriving (Eq, Show)

instance Semigroup (Or a b) where
  Fst x <> Fst _ = Fst x
  Fst _ <> Snd x = Snd x
  Snd x <> _ = Snd x

instance Monoid a => Monoid (Or a b) where
  mempty = Fst mempty
  mappend = (<>)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return $ Fst a), (3, return $ Snd b)]

orAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
orAssoc a b c = (a <> b) <> c == a <> (b <> c)

type OrInt = Or Int Int

type OrIntAssoc = OrInt -> OrInt -> OrInt -> Bool

-- 9
newtype Combine a b = Combine
  { unCombine :: (a -> b)
  }

instance Semigroup b => Semigroup (Combine a b) where
  Combine a <> Combine b = Combine (\x -> a x <> b x)

instance (Semigroup b, Monoid b) => Monoid (Combine a b) where
  mempty = Combine mempty
  mappend = (<>)

-- Needed help here
-- See: https://stackoverflow.com/a/52984192/4955686
monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity m = mappend mempty m == m

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity m = mappend m mempty == m

monoidLeftIdentityF ::
     (Eq b, Monoid m) => (Fun a b -> m) -> (m -> a -> b) -> a -> Fun a b -> Bool
monoidLeftIdentityF wrap eval point candidate =
  eval (mappend mempty m) point == eval m point
  where
    m = wrap candidate

monoidRightIdentityF ::
     (Eq b, Monoid m) => (Fun a b -> m) -> (m -> a -> b) -> a -> Fun a b -> Bool
monoidRightIdentityF wrap eval point candidate =
  eval (mappend m mempty) point == eval m point
  where
    m = wrap candidate

-- 10
newtype Comp a = Comp
  { unComp :: (a -> a)
  }

instance Semigroup a => Semigroup (Comp a) where
  Comp a <> Comp b = Comp (a <> b)

instance (Semigroup a, Monoid a) => Monoid (Comp a) where
  mempty = Comp mempty
  mappend = (<>)

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (semigroupAssoc :: IdentityAssocString)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (semigroupAssoc :: ThreeAssoc)
  quickCheck (semigroupAssoc :: FourAssoc)
  quickCheck (orAssoc :: OrIntAssoc)
  quickCheck $
    (monoidLeftIdentityF (Combine . applyFun) unCombine :: Int -> Fun Int (Sum Int) -> Bool)
  quickCheck $
    (monoidRightIdentityF (Combine . applyFun) unCombine :: Int -> Fun Int (Sum Int) -> Bool)
