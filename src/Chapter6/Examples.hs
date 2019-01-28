module Examples where

check' :: Ord a => a -> a -> Bool -- Ord implies Eq

check' a a' = a == a'

data Mood = Blah
    deriving Show-- instance Show Mood where
--   show _ = "Blah"
