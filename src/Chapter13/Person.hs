module Chapter13.Person where

import           System.IO

type Name = String
type Age = Integer

data Person =
  Person Name
         Age
  deriving (Show)

data PersonInvalid
  = NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise =
    Left $
    PersonInvalidUnknown $ "Name was: " ++ show name ++ " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  hSetBuffering stdout NoBuffering
  putStr "Please input the person name: "
  name <- getLine
  putStr "Please input the person age: "
  age <- getLine
  let person = mkPerson name (read age)
   in case person of
        (Right _) ->
          putStrLn ("Yay! Successfully got a person: " ++ show person)
        (Left NameEmpty) -> putStrLn "Error: name empty"
        (Left AgeTooLow) -> putStrLn "Error: age too low"
        (Left _) -> putStrLn "Error: person invalid"
