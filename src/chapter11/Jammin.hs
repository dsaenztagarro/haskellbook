module Jammin where

import Data.List (groupBy, mapAccumR, maximumBy, sortBy)

data Fruit = Peach
           | Plum
           | Apple
           | Blackberry
           deriving (Eq, Show, Ord)

data JamJars =
  Jam { fruit :: Fruit
      , jars  :: Int }
      deriving (Eq, Show)

instance Ord JamJars where
  compare jamjar1 jamjar2 = compare (jars jamjar1) (jars jamjar2)

row1 = [(Jam Blackberry 1), (Jam Apple 2), (Jam Peach 4)]
row2 = [(Jam Blackberry 10), (Jam Apple 3)]

allJam :: [[JamJars]]
allJam = [row1, row2]

allJam1 :: [JamJars]
allJam1 = concat allJam

total :: Int
total = foldr1 (+) $ map (\jam -> jars jam) allJam1

mostRow :: JamJars
mostRow = maximumBy compare allJam1

sortJam :: [JamJars]
sortJam = sortBy compareKind allJam1

compareKind :: JamJars -> JamJars -> Ordering
compareKind (Jam k _) (Jam k' _) = compare k k'

groupJam :: [[JamJars]]
groupJam = groupBy (\j j' -> (compareKind j j') == EQ) sortJam
