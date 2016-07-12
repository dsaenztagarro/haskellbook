{-# NegativeLiterals #-}

-- src/chapter11/NegativeLiterals.hs:9:19: Warning:
--     Literal 128 is out of the Int8 range -128..127
--     If you are trying to write a large negative literal, use NegativeLiterals
-- Ok, modules loaded: NegativeLiterals.

module NegativeLiterals where

import Data.Int

data NumberOrBool = Numba Int8
                  | BoolyBool Bool
                  deriving (Eq, Show)

myNumba = Numba (-128)

