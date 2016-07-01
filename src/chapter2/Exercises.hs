module Exercises where

-- (2.12) page 82

-- let x = 3; y = 1000 in x * 3 + y

op1 = x * 3 + y
  where x = 3
        y = 1000

-- let y = 10; x = 10 * 5 + y in x * 5

op2 = x * 5
  where y = 10
        x = 10 * 5 * y

-- let x = 7; y = negate x; z = y * 10 in z / x + y

op3 = z / x + y
  where x = 7
        y = negate x
        z = y * 10

-- (2.13) More fun with function

waxOn = x * 5
  where z = 7
        x = y ^ 2
        y = z + 8

triple x = x * 3

waxOff x = x * 3

