-- Book: haskell-programming-0.11.2-screen.pdf
-- Section: Ciphers
-- Page: 474

module Ciphers where

import Data.Char (chr, isUpper, ord)

caesar :: String -> String -> String
caesar = undefined
-- caesar xs n = map (shift n) xs

uncaesar :: String -> String -> String
uncaesar = undefined
-- uncaesar xs n = map (shift (-n)) xs

shift :: Int -> Char -> Char
shift n x = chr $ base + mod (ord x - base + n) 26
  where base = ord $ if isUpper x then 'A' else 'a'

data ReplaceInfo = ReplaceInfo
  { keyword :: String
  , result :: String }

replaceChar :: ReplaceInfo -> Char -> ReplaceInfo
replaceChar (ReplaceInfo key res) char = ReplaceInfo key newResult
  where index = mod (length res) (length key)
        newChar = if char == ' ' then ' ' else (key !! index)
        newResult = res ++ [newChar]

-- replace :: String -> String -> String
-- replace xs k = foldl f (ReplaceInfo k "") xs
--
testReplaceChar1 :: IO ()
testReplaceChar1 =
  if result (replaceChar (ReplaceInfo "ALLY" "") 'M') == "A"
  then putStrLn "ReplaceChar1 OK"
  else putStrLn "ReplaceChar1 ERROR"

testReplaceChar2 :: IO ()
testReplaceChar2 =
  if result (replaceChar (ReplaceInfo "ALLY" "ALL") 'T') == "ALLY"
  then putStrLn "ReplaceChar2 OK"
  else putStrLn "ReplaceChar2 ERROR"

-- testReplace :: IO ()
-- testReplace =
--   if replace "MEET AT DAWN" "ALLY" == "ALLY AL LYAL"
--   then putStrLn "Replace OK"
--   else putStrLn "Replace ERROR"

main :: IO ()
main = do
  testReplaceChar1
  testReplaceChar2
  -- testReplace
  -- testCaesar
  -- testUncaesar
