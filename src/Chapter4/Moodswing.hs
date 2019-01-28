module MoodSwing where

import Data.Scientific as Scientific

data Mood = Blah | Woot
    deriving Show

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood _ = Blah

testManuel :: Scientific -> Scientific
testManuel x = x + x
