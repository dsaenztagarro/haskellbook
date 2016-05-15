module Reverse where

rvrs "" = ""
rvrs xs = word3 ++ " " ++ word2 ++ " " ++ word1
  where word1 = take 5 xs
        word2 = drop 6 $ take 8 xs
        word3 = drop 9 xs

sentence :: String
sentence = "Curry is awesome!"

main :: IO ()
main = print $ rvrs sentence
