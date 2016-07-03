module Fibonacci where

fibs :: [Int]
fibs = 1 : scanl (+) 1 fibs

fibsN :: Int -> Int
fibsN x = fibs !! x

fibs1To20 = take 20 fibs

fibsMinorThan100 = filter (\x -> x < 100) fibs1To20

factorial n = last $ scanl (*) 1 [1..n]

