module Chapter15.MaybeAnotherMonoid where

import Control.Monad
import Data.Monoid
import Test.QuickCheck

data Optional a
  = Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend Nada Nada = Nada
  mappend (Only x) Nada = Only x
  mappend Nada (Only x) = Only x
  mappend (Only x) (Only y) = Only (mappend x y)

getOnly :: Arbitrary a => Gen (Optional a)
getOnly = do
  a <- arbitrary
  return (Only a)

genOptional :: Arbitrary a => Gen (Optional a)
genOptional = frequency [(1, return Nada), (3, getOnly)]

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = genOptional

newtype First' a = First'
  { getFirst' :: Optional a
  } deriving (Eq, Show)

instance Monoid (First' a) where
  mempty = First' Nada
  mappend (First' Nada) (First' Nada) = First' Nada
  mappend (First' Nada) (First' (Only x)) = First' (Only x)
  mappend (First' (Only x)) _ = First' (Only x)

genFirst' :: Arbitrary a => Gen (First' a)
genFirst' = do
  a <- arbitrary
  return First' {getFirst' = a}

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = genFirst'

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String -> First' String -> First' String -> Bool

type FsId = First' String -> Bool

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FsId)
  quickCheck (monoidRightIdentity :: FsId)
