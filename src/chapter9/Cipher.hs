module Cipher where

import Data.Char (chr, isUpper, ord)

caesar :: String -> Int -> String
caesar xs n = map (shift n) xs

uncaesar :: String -> Int -> String
uncaesar xs n = map (shift (-n)) xs

shift :: Int -> Char -> Char
shift n x = chr $ base + mod (ord x - base + n) 26
  where base = ord $ if isUpper x then 'A' else 'a'
