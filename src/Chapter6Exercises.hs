module Chapter6Exercises where

import Data.List (sort)

-- Eq Instances

data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
  (TisAn x) == (TisAn y) = (x == y)

data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
  Two a b == Two c d = (a == c) && (b == d)

data StringOrInt =
    TisAnInt Int
  | TisAString String

instance Eq StringOrInt where
  TisAnInt x == TisAnInt y = (x == y)
  TisAString x == TisAString y = (x == y)
  _ == _ = False

data Pair a = Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair x y) (Pair x' y') = (x == x') && (y == y')

data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple a b) (Tuple a' b') = (a == a') && (b == b')

data Which a =
    ThisOne a
  | ThatOne a

instance Eq a => Eq (Which a) where
  (==) (ThisOne v) (ThisOne v') = v == v'
  (==) (ThatOne v) (ThatOne v') = v == v'
  (==) _ _ = False

data EitherOr a b =
    Hello a
  | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello v) (Hello v') = v == v'
  (==) (Goodbye v) (Goodbye v') = v == v'
  (==) _ _ = False

data DayOfWeek =
  Mon | Tue | Weds | Thu | Fri | Sat | Sun
  deriving (Eq, Show)

instance Ord DayOfWeek where
  compare Fri Fri = EQ
  compare Fri _   = GT
  compare _ Fri   = LT
  compare _ _     = EQ

-- Does it typecheck?

-- 1.

data Person = Person Bool deriving Show

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

-- 2.

data Mood = Blah
          | Woot deriving (Eq, Show)

settleDown :: Mood -> Mood
settleDown x = if x == Woot
                then Blah
                else x

-- 4.

type Subject = String
type Verb = String
type Object = String

data Sentence =
  Sentence Subject Verb Object
  deriving (Eq, Show)

s1 :: Object -> Sentence
s1 = Sentence "dogs" "drool"

s2 :: Sentence
s2 = Sentence "Julie" "loves" "dogs"

-- Given a datatype declaration, what can we do?

data Rocks =
  Rocks String deriving (Eq, Ord, Show)

data Yeah =
  Yeah Bool deriving (Eq, Ord, Show)

data Papu =
  Papu Rocks Yeah
  deriving (Eq, Ord, Show)

phew :: Papu
phew = Papu (Rocks "chases") (Yeah True)

truth :: Papu
truth = Papu (Rocks "chomskydoz")
             (Yeah True)

equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

comparePapus :: Papu -> Papu -> Bool
comparePapus p p' = p > p'

-- Match the types

i :: Num a => a
-- FAIL
-- i :: a
i = 1

f :: Float
-- FAIL
-- f :: Num a => a
-- SUCCESS
-- f :: Fractional a => a
-- SUCCESS
-- f :: RealFrac a => a
f = 1.0

freud :: a -> a
-- SUCCESS
-- freud :: Ord a => a -> a
freud x = x

freud' :: a -> a
-- SUCCESS
-- freud' :: Int -> Int
freud' x = x

myX :: Int
myX = 1 :: Int

sigmund :: Int -> Int
-- FAIL
-- sigmund :: a -> a
-- FAIL
-- sigmund :: Num a => a -> a
sigmund _ = myX

jung :: Ord a => [a] -> a
-- SUCCESS
-- jung :: [Int] -> Int
jung xs = head (sort xs)

young :: [Char] -> Char
-- SUCCESS
-- young :: Ord a => [a] -> a
young xs = head (sort xs)

mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
-- FAIL
-- signifier :: Ord a => [a] -> a
signifier xs = head (mySort xs)

-- Type-Kwon-Do Two: Electric Typealoo
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk h x y = h x == y

arith :: Num b => (a -> b) -> Integer -> a -> b
arith h' z x = (h' x) + fromInteger z
