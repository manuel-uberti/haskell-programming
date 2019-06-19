-- | 
module Chapter15.Test where

import Test.QuickCheck
import Data.Semigroup

data Identity a = Identity a
    deriving ( Eq, Show )

instance Semigroup a => Semigroup (Identity a) where
    (Identity x) <> (Identity y) = Identity (x <> y)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary
        return $ Identity a

semiAssoc :: ( Eq m, Semigroup m ) => m -> m -> m -> Bool
semiAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type IdentAssocString
    = Identity String -> Identity String -> Identity String -> Bool

main :: IO ()
main = do
    quickCheck (semiAssoc :: IdentAssocString)
