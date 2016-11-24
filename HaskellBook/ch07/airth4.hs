module Arith4 where

-- id :: a -> a
-- id x = x

roundTrip' :: (Show a, Read a) => a -> a
roundTrip' a = read (show a)

roundTrip :: (Show a, Read b) => a -> b
roundTrip a = read (show a)

pfRoundTrip :: (Show a, Read a) => a -> a
pfRoundTrip = read . show

main = do
  print (roundTrip (4 :: Int) :: Int)
  print (roundTrip' 4)
  print (id 4)

data Product a b = Product a b

-- unpackProductBad :: Product a b -> (a,b)
-- unpackProductBad (Product x x) = (x, x)  -- Conflicting definitions for 'x'
