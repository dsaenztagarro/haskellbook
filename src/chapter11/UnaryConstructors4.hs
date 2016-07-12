{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

-- src/chapter11/UnaryConstructors4.hs:15:10:
--     Illegal instance declaration for ‘TooMany (Int, String)’
--       (All instance types must be of the form (T a1 ... an)
--        where a1 ... an are *distinct type variables*,
--        and each type variable appears at most once in the instance head.
--        Use FlexibleInstances if you want to disable this.)
--     In the instance declaration for ‘TooMany (Int, String)’
-- Failed, modules loaded: none.

module UnaryConstructors where

newtype Goats = Goats Int deriving (Eq, Show, TooMany)

newtype Cows = Cows Int deriving (Eq, Show)

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

instance TooMany (Int, String) where
  tooMany (n, _) = n > 20

instance TooMany (Int, Int) where
  tooMany (n, m) = n + m > 10

-- TODO
-- instance TooMany ((Num a, TooMany a) => (a, a)) where
--   tooMany (n, m) = (n > 5) && (tooMany m)
