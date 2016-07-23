-- Book: haskell-programming-0.11.2-screen.pdf
-- Section: Ciphers
-- Page: 474

module Ciphers where

import Data.Char (chr, isUpper, ord)

mystr = "MEET AT DAWN"
mykey = "ALLY"

caesar :: String -> String -> String
caesar xs k = map applyShiftr (zip xs keySub)
  where keySub = replace xs k

uncaesar :: String -> String -> String
uncaesar xs k = map applyShiftl (zip xs keySub)
  where keySub = replace xs k

applyShiftr :: (Char, Char) -> Char
applyShiftr (x, y) = if x == ' '
                    then ' '
                    else shift (jump y) x

applyShiftl :: (Char, Char) -> Char
applyShiftl (x, y) = if x == ' '
                    then ' '
                    else shift (-1 * (jump y)) x

shift :: Int -> Char -> Char
shift n x = chr $ base + mod (ord x - base + n) 26
  where base = ord $ if isUpper x then 'A' else 'a'

data ReplaceInfo = ReplaceInfo
  { keyword :: String
  , result :: String }

replaceChar :: ReplaceInfo -> Char -> ReplaceInfo
replaceChar (ReplaceInfo key res) char = ReplaceInfo key newResult
  where resWithoutSpaces = concat $ words res
        index = mod (length resWithoutSpaces) (length key)
        newChar = if char == ' ' then ' ' else (key !! index)
        newResult = res ++ [newChar]

replace :: String -> String -> String
replace xs k = result $ foldl replaceChar (ReplaceInfo k "") xs

jump :: Char -> Int
jump x = ord x - ord 'A'

-- Test cases

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

testReplaceChar3 :: IO ()
testReplaceChar3 =
  if result (replaceChar (ReplaceInfo "ALLY" "ALLY") ' ') == "ALLY "
  then putStrLn "ReplaceChar3 OK"
  else putStrLn "ReplaceChar3 ERROR"

testReplace :: IO ()
testReplace =
  if replace "MEET AT DAWN" "ALLY" == "ALLY AL LYAL"
  then putStrLn "replace OK"
  else putStrLn "replace ErrOr"

testCaesar :: IO ()
testCaesar =
  if caesar "MEET AT DAWN" "ALLY" == "MPPR AE OYWY"
  then putStrLn "caesar OK"
  else putStrLn "caesar ERROR"

testUncaesar :: IO ()
testUncaesar =
  if uncaesar "MPPR AE OYWY" "ALLY" == "MEET AT DAWN"
  then putStrLn "uncaesar OK"
  else putStrLn "uncaesar ERROR"

main :: IO ()
main = do
  testReplaceChar1
  testReplaceChar2
  testReplaceChar3
  testReplace
  testCaesar
  testUncaesar
  -- testUncaesar
