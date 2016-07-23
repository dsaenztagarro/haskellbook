-- Book: haskell-programming-0.11.2-screen.pdf
-- Section: As-Patterns
-- Page: 475

module AsPatterns where

import Data.Char (toUpper)

capitalizeWords :: String -> [(String, String)]
capitalizeWords s = map f (words s)
  where f w@(x:xs) = (w, toUpper(x) : xs)
        f [] = ("","")

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf xs'@(x:xs) (y:ys) = if x == y
                                       then isSubsequenceOf xs ys
                                       else isSubsequenceOf xs' ys
isSubsequenceOf _ [] = False