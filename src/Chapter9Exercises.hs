module Chapter9Exercises where

-- Thy Fearful Symmetry

myWords :: String -> [String]
myWords xs = go xs []
  where go [] memo = memo
        go zs memo = go (next zs) (memo ++ [readWord zs])
        next = dropBlanks . dropWord

readWord :: String -> String
readWord ys = takeWhile isLetter ys

dropWord :: String -> String
dropWord ys = dropWhile isLetter ys

dropBlanks:: String -> String
dropBlanks ys = dropWhile isBlank ys

isLetter :: Char -> Bool
isLetter x = x /= ' '

isBlank :: Char -> Bool
isBlank x = x == ' '
