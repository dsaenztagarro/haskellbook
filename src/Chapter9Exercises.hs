module Chapter9Exercises where

import Data.Char

-- Thy Fearful Symmetry

myWords :: String -> [String]
myWords xs = go xs []
  where go [] memo = memo
        go zs memo = go (next zs) (memo ++ [readWord zs])
        next = dropBlanks . dropWord

readWord :: String -> String
readWord ys = takeWhile isLetter' ys

dropWord :: String -> String
dropWord ys = dropWhile isLetter' ys

dropBlanks:: String -> String
dropBlanks ys = dropWhile isBlank ys

isLetter' :: Char -> Bool
isLetter' x = x /= ' '

isBlank :: Char -> Bool
isBlank x = x == ' '

-- Filtering

multiplesOf3 :: [Int] -> [Int]
multiplesOf3 xs = filter isMultipleOf3 xs
  where isMultipleOf3 x = rem x 3 == 0

countMultiplesOf3 :: [Int] -> Int
countMultiplesOf3 = length . multiplesOf3

removeArticles :: String -> String
removeArticles xs = unwords [x | x <- words xs, not (isArticle x)]
  where isArticle x = elem x ["the", "a", "an"]

-- Zipping

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith f xs ys

zip'' :: [a] -> [b] -> [(a,b)]
zip'' xs ys = zipWith (\x y -> (x,y)) xs ys

-- Data.Char

filterUpper :: String -> String
filterUpper = filter isUpper

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : xs

capitalizeAll :: String -> String
capitalizeAll [] = []
capitalizeAll (x:xs) = toUpper x : capitalizeAll xs

capHead :: String -> Char
capHead = toUpper . head
