module Arith4 where

roundTrip :: (Read a, Show a) => a -> a
roundTrip a = read (show a)

roundTrip1 :: (Read a, Show a) => a -> a
roundTrip1 = read . show

main :: IO ()
main = do
  print (roundTrip 4)
  print (roundTrip1 11)
  print (id 4)
