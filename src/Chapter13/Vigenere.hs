module Chapter13.Vigenere where

import Data.Char
import System.IO

baseOrd :: Int
baseOrd = ord 'A'

keyword :: String
keyword = "ALLY"

transcode :: String -> String -> String
transcode keyword text = take l $ cycle keyword
  where
    l = length $ concat $ words text

shiftIdxs keyword text = map (\x -> mod (ord x) baseOrd)
    $ transcode keyword text

shiftRight :: Char -> Int -> Char
shiftRight c x = if mod idx baseOrd > 25 then chr $ baseOrd
    + (mod idx baseOrd - 26) else chr idx
  where
    idx = ord c + x

shiftLeft :: Char -> Int -> Char
shiftLeft c x = if mod idx baseOrd > 25 then chr (mod idx baseOrd + 26)
    else chr idx
  where
    idx = ord c - x

vigenereEncode :: String -> String -> String
vigenereEncode keyword text = zipWith shiftRight ws idxs
  where
    ws = concat $ words text

    idxs = shiftIdxs keyword text

vigenereDecode :: String -> String -> String
vigenereDecode keyword text = zipWith shiftLeft ws idxs
  where
    ws = concat $ words text

    idxs = shiftIdxs keyword text

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStr "Please input the secret keyword: "
    k <- getLine
    putStr "Please input the string to cipher: "
    s <- getLine
    putStrLn $ vigenereEncode k s
