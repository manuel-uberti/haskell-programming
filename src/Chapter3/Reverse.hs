module Reverse where

rvrs :: String -> String
rvrs x = a ++ " " ++ b ++ " " ++ c
  where
    a = drop 9 x
    b = drop 6 (take 8 x)
    c = take 5 x

main :: IO ()
main = print $ rvrs "Curry is awesome" -- or print (rvrs "Curry is awesome")

rvrs' :: String -> String
rvrs' xs = a ++ " " ++ b ++ " " ++ c
  where
    w = words xs
    a = w !! 2
    b = w !! 1
    c = w !! 0
