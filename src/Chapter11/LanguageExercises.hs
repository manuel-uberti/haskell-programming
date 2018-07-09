module Chapter11.LanguageExercises where

import Data.Char
import Data.List

-- 1
capitalizeWord :: String -> String
capitalizeWord s = [toUpper (head s)] ++ tail s


-- 2
endsWith :: Char -> String -> Bool
endsWith _ [] = False
endsWith a xs =
  if a == last xs
    then True
    else False

capitalizeParagraph :: String -> String
capitalizeParagraph [] = []
capitalizeParagraph s =
  concat $ intersperse " " $ go ws [capitalizeWord (head ws)]
  where
    ws = words s
    go ws acc =
      foldr
        (\a b ->
           if endsWith '.' (last b)
             then b ++ [capitalizeWord a]
             else b ++ [a])
        acc
        (reverse (tail ws))
