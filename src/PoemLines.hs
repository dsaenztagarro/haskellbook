module PoemLines where

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen
					++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines xs = go xs []
  where go [] memo = memo
        go zs memo = go (next zs) (memo ++ [readSentence zs])
        next = dropBreaklines . dropSentence

readSentence :: String -> String
readSentence ys = takeWhile isSentence ys

dropSentence :: String -> String
dropSentence ys = dropWhile isSentence ys

dropBreaklines :: String -> String
dropBreaklines ys = dropWhile isBreakline ys

isBreakline :: Char -> Bool
isBreakline x = x == '\n'

isSentence :: Char -> Bool
isSentence = not . isBreakline

shouldEqual =
	[ "Tyger Tyger, burning bright"
	, "In the forests of the night"
	, "What immortal hand or eye"
	, "Could frame thy fearful symmetry?"
	]

main :: IO ()
main =
	print $ "Are they equal?" ++ show (myLines sentences == shouldEqual)
