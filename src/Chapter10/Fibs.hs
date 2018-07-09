module Chapter10.Fibs where

fibs :: [Int]
fibs = 1 : scanl (+) 1 fibs

fibsN :: Int -> Int
fibsN x = fibs !! x


-- Scan Exercises

-- 1
fibs1 :: [Int]
fibs1 = take 20 fibs

-- 2
fibs2 :: [Int]
fibs2 = take 10 (filter (<100) fibs)

-- 3
factorial :: Int -> Int
factorial n = (head . reverse) (scanl (*) 1 [1..n])
