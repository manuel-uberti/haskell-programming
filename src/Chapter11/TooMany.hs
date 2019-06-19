{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Chapter11.TooMany where

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats =
  Goats Int
  deriving (Eq, Show, TooMany)

-- instance TooMany Goats where
--   tooMany (Goats n) = tooMany n
-- Exercises: Logic Goats
-- 1
instance TooMany (Int, String) where
  tooMany (x, y) = x > 42

-- 2
instance TooMany (Int, Int) where
  tooMany (x, y) = (x + y) > 84

-- 3
instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (x, y) = tooMany (x + y)
