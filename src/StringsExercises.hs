module StringsExercises where

exclamation :: String -> String
exclamation xs = xs ++ ['!']

fifthLetter :: String -> Char
fifthLetter xs = xs !! 4

fromPos9 :: String -> String
fromPos9 xs = drop 9 xs

thirdLetter :: String -> Char
thirdLetter xs = xs !! 2

letterIndex :: Int -> Char
letterIndex x = xs !! x
  where xs = "Curry is awesome!"
