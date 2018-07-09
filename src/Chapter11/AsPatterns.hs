module Chapter11.AsPatterns where

import Data.Char

-- 1
isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf xs@(x:_) ys@(y:_) =
  xs == ys || (elem x ys && isSubseqOf (tail xs) (tail ys))


-- 2
capitalizeWord :: String -> String
capitalizeWord s = [toUpper (head s)] ++ tail s

capitalizeWords :: String -> [(String, String)]
capitalizeWords xs@(x:_) = [(x, capitalizeWord x) | x <- ws]
  where
    ws = words xs
