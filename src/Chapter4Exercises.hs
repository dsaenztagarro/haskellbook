-- Chapter 4. Basic Data Types
module Chapter4Exercises where

data Mood = Blah | Woot deriving Show

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood Woot = Blah

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x

myAbs :: Integer -> Integer
myAbs x = if x < 0 then negate x else x

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = ((snd x, snd y), (fst x, fst y))

op = (+ 1)

g xs = op w
  where w = length xs

-- (\(x:xs) -> x) [3,4,5]

-- h (a, b) = a