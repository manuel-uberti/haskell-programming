module Chapter13.Cipher where

import Data.Char
import System.IO

baseOrd :: Int
baseOrd = ord 'a'

shiftRight :: Char -> Int -> Char
shiftRight c x =
  if mod idx baseOrd > 25
    then chr (baseOrd + (mod idx baseOrd - 26))
    else chr idx
  where
    idx = ord c + x

caesar :: String -> Int -> String
caesar [] _ = []
caesar s x = [shiftRight (head s) x] ++ caesar (tail s) x

shiftLeft :: Char -> Int -> Char
shiftLeft c x =
  if mod idx baseOrd > 25
    then chr (mod idx baseOrd + 26)
    else chr idx
  where
    idx = ord c - x

unCaesar :: String -> Int -> String
unCaesar [] _ = []
unCaesar s x = [shiftLeft (head s) x] ++ unCaesar (tail s) x

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStr "Please input the string to cipher: "
  s <- getLine
  putStr "Please input the number of shifts: "
  n <- getLine
  putStrLn $ caesar s $ read n
