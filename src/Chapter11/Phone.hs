module Chapter11.Phone where

import Data.Char
import Data.List

-- 1
data DaPhone =
  DaPhone [(Char, String)]
  deriving (Eq, Ord, Show)

phone :: DaPhone
phone =
  DaPhone
    [ ('1', ['1'])
    , ('2', ['A', 'B', 'C', '2'])
    , ('3', ['D', 'E', 'F', '3'])
    , ('4', ['G', 'H', 'I', '4'])
    , ('5', ['J', 'K', 'L', '5'])
    , ('6', ['M', 'N', 'O', '6'])
    , ('7', ['P', 'Q', 'R', 'S', '7'])
    , ('8', ['T', 'U', 'V', '8'])
    , ('9', ['W', 'X', 'Y', 'Z', '9'])
    , ('*', ['^'])
    , ('0', ['+', '_', '0'])
    , ('#', ['.', ','])
    ]

-- 2
convo :: [String]
convo =
  [ "Wanna play 20 questions"
  , "Ya"
  , "U 1st haha"
  , "Lol ok. Have u ever tasted alcohol"
  , "Lol ya"
  , "Wow ur cool haha. Ur turn"
  , "Ok. Do u think I am pretty Lol"
  , "Lol ya"
  , "Just making sure rofl ur turn"
  ]

type Digit = Char

type Presses = Int

buttons :: DaPhone -> [(Char, String)]
buttons (DaPhone bs) = bs

button :: DaPhone -> Char -> (Char, String)
button p ' ' = ('0', ['+', '_'])
button p c = head $ filter (\x -> elem (toUpper c) (snd x)) $ buttons p

presses :: (Char, String) -> Char -> Int
presses b c = (+ 1) $ length $ takeWhile (\x -> x /= (toUpper c)) (snd b)

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps p c
  | c == toUpper c = [('*', 1), (f, ps)]
  | otherwise = [(f, ps)]
  where
    b = button p c
    f = fst b
    ps = presses b c

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead p s = foldr (\a b -> reverseTaps p a ++ b) [] s

-- 3
fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps ds = foldr (+) 0 $ map snd ds

-- 4
removeSpaces :: String -> String
removeSpaces s = filter (\x -> x /= ' ') s

groupChars :: String -> [String]
groupChars s = map (\x -> filter (\y -> y == x) s) s

uniqueChars :: [String] -> [String]
uniqueChars xs =
  foldr
    (\a b ->
       if elem a b
         then b
         else a : b)
    []
    xs

longestCharSeq :: [String] -> String
longestCharSeq (x:xs) =
  foldl
    (\a b ->
       if length a > length b
         then a
         else b)
    x
    xs

mostPopularLetter :: String -> Char
mostPopularLetter s =
  head $ longestCharSeq $ uniqueChars $ groupChars $ removeSpaces s

-- 5
coolestLtr :: [String] -> Char
coolestLtr xs = mostPopularLetter $ map mostPopularLetter xs

flatConv :: [String] -> [String]
flatConv xs = concat $ map words xs

groupWords :: [String] -> [[String]]
groupWords xs = map (\x -> filter (\y -> y == x) xs) xs

uniqueWords :: [[String]] -> [[String]]
uniqueWords xs =
  foldr
    (\a b ->
       if elem a b
         then b
         else a : b)
    []
    xs

longestWord :: [[String]] -> [String]
longestWord (x:xs) =
  foldl
    (\a b ->
       if length a > length b
         then a
         else b)
    x
    xs

coolestWord :: [String] -> String
coolestWord xs = head $ longestWord $ uniqueWords $ groupWords $ flatConv xs
