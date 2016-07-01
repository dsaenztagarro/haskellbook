module MyStandardFunctions where

myOr :: [Bool] -> Bool
myOr [] = True
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = f x || myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem y (x:xs) = (y == x) || myElem y xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f xs = go f (tail xs) (head xs)
  where go _ [] m = m
        go g (y:ys) m = go g ys (if g m y == GT then m else y)

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f xs = go f (tail xs) (head xs)
  where go _ [] m = m
        go g (y:ys) m = go g ys (if g m y == LT then m else y)

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
