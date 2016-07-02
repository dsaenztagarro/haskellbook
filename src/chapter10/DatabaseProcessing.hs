module DatabaseProcessing where

import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbNumber 10
  , DbString "Hello, world!"
  , DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr accumulate []
  where accumulate (DbDate time) xs = time : xs
        accumulate _ xs = xs

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr accumulate []
  where accumulate (DbNumber value) xs = value : xs
        accumulate _ xs = xs

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent db = head $ foldr accumulate [] db
  where accumulate (DbDate value) [] = [value]
        accumulate (DbDate value) [time] = if value > time then [value] else [time]
        accumulate _ xs = xs

sumDb :: [DatabaseItem] -> Integer
sumDb = foldr accumulate 0
  where accumulate (DbNumber value) memo = memo + value
        accumulate _ memo = memo

avgDb :: [DatabaseItem] -> Double
avgDb db = (fromIntegral (sum numbers)) / fromIntegral (length numbers)
  where numbers = filterDbNumber db
