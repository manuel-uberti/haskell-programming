module Chapter13.Palindrome where

import Control.Monad
import Data.Char (toLower)
import System.Exit (exitSuccess)

punctuation :: [Char]
punctuation = ['.', ',', '.', ';', '!', '?', '\'']

alphabetChar :: Char -> Bool
alphabetChar c = not (elem c punctuation)

removePunctuation :: String -> String
removePunctuation s = [x | x <- s, alphabetChar x]

normalizeInput :: String -> String
normalizeInput s = map toLower $ concat $ words $ removePunctuation s

palindrome :: IO ()
palindrome =
  forever $ do
    line1 <- getLine
    let nl = normalizeInput line1
     in if (nl == reverse nl)
          then putStrLn "It's a palindrome!"
          else do
            putStrLn "Nope"
            exitSuccess
