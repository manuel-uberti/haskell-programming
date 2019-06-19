module Chapter11.Exercises where

import Data.Int

-- Vehicles
data Price =
  Price Integer
  deriving (Eq, Show)

data Manufacturer
  = Mini
  | Mazda
  | Tata
  deriving (Eq, Show)

data Airline
  = PapuAir Integer
  | CatapultsR'Us Integer
  | TakeYourChancesUnited Integer
  deriving (Eq, Show)

data Vehicle
  = Car Manufacturer
        Price
  | Plane Airline
  deriving (Eq, Show)

-- 1
-- All of types Vehicle
myCar = Car Mini (Price 14000)

urCar = Car Mazda (Price 20000)

clownCar = Car Tata (Price 7000)

doge = Plane (PapuAir 100)

-- 2
isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars xs = map isCar xs

-- 3
getManu :: Vehicle -> Manufacturer
getManu (Car x _) = x

-- 4
-- getManu for Plane returns an exception, it is an incomplete pattern matching
-- 5
-- added Integer to Plane data constructors to represent size
-- Pity the Bool
-- 1
data BigSmall
  = Big Bool
  | Small Bool
  deriving (Eq, Show) -- cardinality: 1 + 2 + 1 + 2 = 6

-- 2
data NumberOrBool
  = Numba Int8
  | BoolyBool Bool
  deriving (Eq, Show)

myNumba = Numba (-128)

-- cardinality: 1 + 256 + 1 + 2 = 260
-- values < -128 and > 127 are out of the Int8 range -128..127
-- How Does Your Garden Grow?
-- 1
type Gardener = String

data GardenNormal
  = Gardenia Gardener
  | Daisy Gardener
  | Rose Gardener
  | Lilac Gardener
  deriving (Show)

-- Programmers
data OperatingSystem
  = GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgLang
  = Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show)

data Programmer = Programmer
  { os :: OperatingSystem
  , lang :: ProgLang
  } deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [GnuPlusLinux, OpenBSDPlusNevermindJustBSDStill, Mac, Windows]

allLanguages :: [ProgLang]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers =
  [Programmer os l | os <- allOperatingSystems, l <- allLanguages]

-- Function type is exponential
data Quantum
  = Yes
  | No
  | Both
  deriving (Eq, Show)

convert0 :: Quantum -> Bool
convert0 Yes = True
convert0 No = False
convert0 Both = False

convert1 :: Quantum -> Bool
convert1 Yes = False
convert1 No = True
convert1 Both = False

convert2 :: Quantum -> Bool
convert2 Yes = False
convert2 No = False
convert2 Both = True

convert3 :: Quantum -> Bool
convert3 Yes = True
convert3 No = True
convert3 Both = False

convert4 :: Quantum -> Bool
convert4 Yes = True
convert4 No = False
convert4 Both = True

convert5 :: Quantum -> Bool
convert5 Yes = False
convert5 No = True
convert5 Both = True

convert6 :: Quantum -> Bool
convert6 Yes = True
convert6 No = True
convert6 Both = True

convert7 :: Quantum -> Bool
convert7 Yes = False
convert7 No = False
convert7 Both = False

-- The Quad
data Quad
  = One
  | Two
  | Three
  | Four
  deriving (Eq, Show) -- 1
-- eQuad :: Either Quad Quad
-- 8
-- prodQuad :: (Quad, Quad)
-- 16
-- funcQuad :: Quad -> Quad
-- 256
-- prodToBool :: (Bool, Bool, Bool)
-- 8
-- gTwo :: Bool -> Bool -> Bool
-- 16
-- fTwo :: Bool -> Quad -> Quad
-- 65536
