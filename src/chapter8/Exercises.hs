module Exercises where

cattyConny :: String -> String -> String
cattyConny x y = x ++ "mrow" ++ y

-- fill in the types
flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

-- Recursion

sumAll :: (Integral a) => a -> a
sumAll x = go x
  where go count
          | count == 0 = 0
          | otherwise = count + go (count - 1)

multiply :: (Integral a) => a -> a -> a
multiply x y = go x y 0
  where go _ 0 memo = memo
        go x' y' memo = go x' (y' - 1) (memo + x')

mc91 :: (Integral a) => a -> a
mc91 x
  | x > 100 = x - 10
  | otherwise = 91
