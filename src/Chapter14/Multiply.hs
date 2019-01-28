module Chapter14.Multiply where

import Test.Hspec

f :: ( Eq a, Num a ) => a -> a -> a
f x 1 = x
f x y = x + f x (y - 1)

main :: IO ()
main = hspec $ do
    describe "Multiply" $ do
        it "3 multiplied by 1 is 3" $ do
            f 3 1 `shouldBe` 3
        it "4 multiplied by 3 is 12" $ do
            f 4 3 `shouldBe` 12
