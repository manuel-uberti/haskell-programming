module Exercises where

exclamation :: String -> String
exclamation s = s ++ "!"

fourth :: String -> Char
fourth s = s !! 3

drop9 :: String -> String
drop9 s = drop 9 s

thirdLetter :: String -> Char
thirdLetter s = s !! 2

letterIndex :: Int -> Char
letterIndex x = s !! x
  where s :: String
        s = "Curry is awesome"

rvrs = a ++ " " ++ b ++ " " ++ c
  where s = "Curry is awesome"
        a = drop 9 s
        b = drop 6 (take 8 s)
        c = take 5 s
