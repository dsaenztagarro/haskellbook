module UnderstandingFolds where

fixa = foldr (++) "David" ["woot", "WOOT", "woot"]
fixb = foldr max 'a' "fear is the little death"
fixc = foldr (&&) True [False, True]
fixe = foldl (flip ((++) . show)) "" [1..5]
fixf = foldr (flip const) 'a' [1..5]
fixg = foldr (flip const) 0 "tacos"
fixh = foldl const 0 "burritos"
fixi = foldl const 'z' [1..5]
