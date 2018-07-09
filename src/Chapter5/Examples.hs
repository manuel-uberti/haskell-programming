module Example where

addStuff :: Integer -> Integer -> Integer
addStuff a b = a + b + 5

substractStuff :: Integer -> Integer -> Integer
substractStuff x y = x - y - 10
substractOne = substractStuff 1

nonsense :: Bool -> Integer
nonsense True = 805
nonsense False = 31337

curriedFunction :: Integer -> Bool -> Integer
curriedFunction i b = i + (nonsense b)

uncurriedFunction :: (Integer, Bool) -> Integer
uncurriedFunction (i, b) = i + (nonsense b)

anonymous :: Integer -> Bool -> Integer
anonymous = \i b -> i + (nonsense b)

anonNested :: Integer -> Bool -> Integer
anonNested = \i -> \b -> i + (nonsense b)
