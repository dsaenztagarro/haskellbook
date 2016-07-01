module PoemLinesRefactor where

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen
          ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines xs = go xs []
  where go [] memo = memo
        go zs memo = go (next zs) (memo ++ [readBlock zs])
          where next = dropSeparators . dropBlock

separator :: Char
separator = '\n'

readBlock :: String -> String
readBlock = takeWhile isBlock

dropBlock :: String -> String
dropBlock = dropWhile isBlock

dropSeparators :: String -> String
dropSeparators ys = dropWhile isSeparator ys

isSeparator :: Char -> Bool
isSeparator x = x == separator

isBlock :: Char -> Bool
isBlock = not . isSeparator

shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]

main :: IO ()
main =
  print $ "Are they equal?" ++ show (myLines sentences == shouldEqual)
