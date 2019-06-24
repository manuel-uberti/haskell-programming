module Chapter16.Exercises where

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
