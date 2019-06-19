module Chapter11.Examples where

data Person =
  MkPerson String
           Int
  deriving (Eq, Show)

jm = MkPerson "julie" 108

ca = MkPerson "chris" 16

namae :: Person -> String
namae (MkPerson s _) = s

data PersonRec = PersonRec
  { name :: String
  , age :: Int
  } deriving (Eq, Show)

jmRec = PersonRec "julie" 108

caRec = PersonRec "ca" 16

-- name jmRec
-- age caRec
-- data Fiction = Fiction deriving Show
-- data Nonfiction = Nonfiction deriving Show
data BookType
  = FictionBook
  | NonfictionBook
  deriving (Show)

type AuthorName = String

data Author
  = Fiction AuthorName
  | Nonfiction AuthorName
  deriving (Eq, Show)

data GuessWhat =
  Chickenbutt
  deriving (Eq, Show)

data Id a =
  MkId a
  deriving (Eq, Show)

data Product a b =
  Product a
          b
  deriving (Eq, Show)

data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

data RecordProduct a b = RecordProduct
  { pfirst :: a
  , psecond :: b
  } deriving (Eq, Show)

idInt :: Id Integer
idInt = MkId 10

data Twitter =
  Twitter
  deriving (Eq, Show)

data AskFm =
  AskFm
  deriving (Eq, Show)

socialNetwork :: Sum Twitter AskFm
socialNetwork = First Twitter
