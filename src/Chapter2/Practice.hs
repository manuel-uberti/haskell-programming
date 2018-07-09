module Practice where

-- let x = 3; y = 1000 in x * 3 + y
mult1 = x * 3 + y
  where x = 3
        y = 1000

-- let y = 10; x = 10 * 5 + y in x * 5
mult2 = x * 5
  where y = 10
        x = 10 * 5 + y

-- let x = 7
--     y = negate x
--     z = y * 10
-- in z / x + y
op3 = z / x + y
  where x = 7
        y = negate x
        z = y * 10

waxOn = x * 5
  where z = 7
        y = z + 8
        x = y ^ 2

triple x = x * 3

waxOff x = div (triple x) 10
