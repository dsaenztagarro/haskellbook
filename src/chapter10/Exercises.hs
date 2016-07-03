module Exercises where

-- Warm-up and review

stops  :: [Char]; stops  = "pbtdkg"
vowels :: [Char]; vowels = "aeiou"

tuples3 :: [(Char, Char, Char)]
tuples3 = [(x,y,z) | x <- stops, y <- vowels, z <- stops]

tuplesWithAp :: [(Char, Char, Char)]
tuplesWithAp = filter go tuples3
  where go ('p', _, _) = True
        go _ = False

nouns :: [String]
nouns = ["issue", "developer", "project"]

verbs :: [String]
verbs = ["start", "close", "reopen"]

phrase3 :: [(String, String, String)]
phrase3 = [(x,y,z) | x <- nouns, y <- verbs, z <- nouns]

seekritFunc :: Fractional a => String -> a
seekritFunc x = (fromIntegral (sum (map length (words x)))) / (fromIntegral (length (words x)))

-- Rewriting functions using folds

-- direct recursion, not using (||)
myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = if x == True then True else myOr xs

-- direct recursion, using (||)
myOr2 :: [Bool] -> Bool
myOr2 [] = False
myOr2 (x:xs) = x || myOr xs

-- fold, not point-free in the folding function
myOr3 :: [Bool] -> Bool
myOr3 = foldr (\a b -> if a == False then False else b) True

-- fold, both myOr and the folding function are point-free now
myOr4 :: [Bool] -> Bool
myOr4 = foldr (||) True

myAny :: (a -> Bool) -> [a] -> Bool
myAny p = foldr (\x b -> if p x then True else b) False

myElem :: Eq a => a -> [a] -> Bool
myElem x = any (\y -> x == y)

myReverse :: [a] -> [a]
myReverse = foldl (\b x -> x : b) []

myMap :: (a -> b) -> [a] -> [b]
myMap f  = foldr (\x b -> (f x) : b) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x b -> if f x then x : b else b) []

squish :: [[a]] -> [a]
squish xs = foldr (\x b -> flatten x b) [] xs
  where flatten w m = foldr (\x b -> x : b) m w

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . (map f)

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy p xs = foldr (\x b -> if p x b == GT then x else b) (last xs) xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy p xs = foldr (\x b -> if p x b == LT then x else b) (last xs) xs


