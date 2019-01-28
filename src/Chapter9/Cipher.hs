module Chapter9.Cipher where

import Data.Char

baseOrd :: Int
baseOrd = ord 'a'

shiftRight :: Char -> Int -> Char
shiftRight c x = if mod idx baseOrd > 25 then chr
    (baseOrd + (mod idx baseOrd - 26)) else chr idx
  where
    idx = ord c + x

caesar :: String -> Int -> String
caesar [] _ = []
caesar s x = [ shiftRight (head s) x ] ++ caesar (tail s) x

shiftLeft :: Char -> Int -> Char
shiftLeft c x = if mod idx baseOrd > 25 then chr (mod idx baseOrd + 26)
    else chr idx
  where
    idx = ord c - x

unCaesar :: String -> Int -> String
unCaesar [] _ = []
unCaesar s x = [ shiftLeft (head s) x ] ++ unCaesar (tail s) x
