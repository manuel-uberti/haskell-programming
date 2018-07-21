module Chapter8.Exercises where


-- Reviewing currying

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appendCatty :: String -> String
appendCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"


-- Recursion

-- 2
f :: (Eq a, Num a) => a -> a
f 1 = 1
f x = x + f (x - 1)


-- 3
g :: (Integral a) => a -> a -> a
g x 1 = x
g x y = x + g x (y - 1)


-- 4
data DividedResult = Result Integer | DividedByZero deriving Show

dividedBy :: Integral a => a -> a -> (DividedResult, a)
dividedBy num 0 = (DividedByZero, 0)
dividedBy 0 denom = (Result 0, 0)
dividedBy num denom = go (abs num) (abs denom) 0
  where go n d count
          | n < d = if num < 0 || denom < 0
                    then (Result (- count), n)
                    else (Result count, n)
          | otherwise = go (n - d) d (count + 1)


-- 5
mc91 :: (Integral a) => a -> a
mc91 x = if x > 100 then x - 10 else mc91(mc91 (x + 11))
