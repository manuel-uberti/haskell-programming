module Chapter14.Arithmetic where

import Data.List (sort)
import Test.QuickCheck

-- 1
half :: Fractional a => a -> a
half x = x / 2

halfIdentity :: Fractional a => a -> a
halfIdentity = (* 2) . half

floatGen :: Gen Float
floatGen = arbitrary

prop_halfIdentity :: Property
prop_halfIdentity = forAll floatGen (\x -> x == halfIdentity x)

-- 2
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where
    go _ status@(_, False) = status
    go y (Nothing, t) = (Just y, t)
    go y (Just x, t) = (Just y, x >= y)

genListInt :: Gen [Int]
genListInt = arbitrary

prop_listOrdered :: Property
prop_listOrdered = forAll genListInt (\xs -> (listOrdered $ sort xs) == True)

-- 3
plusAssociative :: (Num a, Eq a) => a -> a -> a -> Bool
plusAssociative x y z = x + (y + z) == (x + y) + z

plusCommutative :: (Num a, Eq a) => a -> a -> Bool
plusCommutative x y = x + y == y + x

genInt :: Gen Int
genInt = arbitrary

prop_plusAssociative :: Property
prop_plusAssociative = forAll genInt (\x y z -> plusAssociative x y z)

prop_plusCommutative :: Property
prop_plusCommutative = forAll genInt (\x y -> plusCommutative x y)

-- 4
timesAssociative :: (Num a, Eq a) => a -> a -> a -> Bool
timesAssociative x y z = x * (y * z) == (x * y) * z

timesCommutative :: (Num a, Eq a) => a -> a -> Bool
timesCommutative x y = x * y == y * x

prop_timesAssociative :: Property
prop_timesAssociative = forAll genInt (\x y z -> timesAssociative x y z)

prop_timesCommutative :: Property
prop_timesCommutative = forAll genInt (\x y -> timesCommutative x y)

-- 5
genIntegral :: Gen (Positive Integer)
genIntegral = arbitrary

quotRemFirstLaw :: Integral a => a -> a -> Bool
quotRemFirstLaw x y = (quot x y) * y + (rem x y) == x

prop_quotRemFirstLaw :: Property
prop_quotRemFirstLaw =
  forAll genIntegral (\x y -> quotRemFirstLaw (getPositive x) (getPositive y))

-- 6
powerAssociative :: (Integral a, Eq a) => a -> a -> a -> Bool
powerAssociative x y z = x ^ (y ^ z) == (x ^ y) ^ z

powerCommutative :: (Integral a, Eq a) => a -> a -> Bool
powerCommutative x y = x ^ y == y ^ x

prop_powerAssociative :: Property
prop_powerAssociative = forAll genInt (\x y z -> powerAssociative x y z)

prop_powerCommutative :: Property
prop_powerCommutative = forAll genInt (\x y -> powerCommutative x y)

-- 7
reverseTwice :: Eq a => [a] -> Bool
reverseTwice xs = (reverse . reverse $ xs) == id xs

prop_reverseTwice :: Property
prop_reverseTwice = forAll genListInt (\xs -> reverseTwice xs)

main :: IO ()
main = do
  quickCheck prop_halfIdentity
  quickCheck prop_listOrdered
  quickCheck prop_plusAssociative
  quickCheck prop_plusCommutative
  quickCheck prop_timesAssociative
  quickCheck prop_timesCommutative
  quickCheck prop_quotRemFirstLaw
  quickCheck prop_powerAssociative
  quickCheck prop_powerCommutative
  quickCheck prop_reverseTwice
