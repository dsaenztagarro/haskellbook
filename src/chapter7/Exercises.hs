module Exercises where

addOneIfOdd :: (Integral a) => a -> a
addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  where f = (\x -> x + 1)

addFive :: Integer -> Integer -> Integer
addFive = (\x y -> (if x > y then y else x) + 5) :: Integer -> Integer -> Integer

mflip :: (t1 -> t2 -> a) -> t2 -> t1 -> a
mflip f x y = f y x

functionC :: (Ord a) => a -> a -> a
functionC x y =
  case comparison of
    True -> x
    False -> y
  where comparison = x > y

-- Exercises: Case Practice

ifEvenAdd2 :: (Integral a) => a -> a
ifEvenAdd2 n =
  case isEven of
    True -> n + 2
    False -> n
  where isEven = even n

nums :: (Num a, Num a1, Ord a) => a -> a1
nums x =
  case compare x 0 of
    LT -> -1
    EQ -> 0
    GT -> 1

-- Exercises: Artful Dodgy

dodgy :: (Num a) => a -> a -> a
dodgy x y = x + y * 10

oneIsOne :: (Num a) => a -> a
oneIsOne = dodgy 1

oneIsTwo :: (Num a) => a -> a
oneIsTwo = (flip dodgy) 2

-- Chapter Exercises: Let's write code

tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = x `div` 10
        d = xLast `mod` 10

tensDigit' :: Integral a => a -> a
tensDigit' x = d
  where (xLast,_) = x `divMod` 10
        (_,d) = xLast `divMod` 10

hunsD :: Integral a => a -> a
hunsD x = d
  where (xLast,_) = x `divMod` 100
        (_,d) = xLast `divMod` 10

foldBold1 :: a -> a -> Bool -> a
foldBold1 x y flag
  | flag == True = x
  | otherwise = y

foldBold2 :: a -> a -> Bool -> a
foldBold2 x y flag =
  case flag of
    True -> x
    False -> y

g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)
