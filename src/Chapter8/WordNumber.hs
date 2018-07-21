module Chapter8.WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n
  | n == 0 = "zero"
  | n == 1 = "one"
  | n == 2 = "two"
  | n == 3 = "three"
  | n == 4 = "four"
  | n == 5 = "five"
  | n == 6 = "six"
  | n == 7 = "seven"
  | n == 8 = "eight"
  | n == 9 = "nine"
  | otherwise = "zero"

digits :: Int -> [Int]
digits n =
  if n < 10
  then [n]
  else digits (div n 10) ++ digits (mod n 10)

wordNumber :: Int -> String
wordNumber n = concat (intersperse "-" (map digitToWord (digits n)))
