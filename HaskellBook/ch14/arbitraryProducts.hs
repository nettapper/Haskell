module Main where

import Test.QuickCheck

data Pair a b =
  Pair a b
  deriving (Show, Eq)

pairGen :: (Arbitrary a, Arbitrary b) =>
           Gen (Pair a b)
pairGen = do
  a <- arbitrary
  b <- arbitrary
  return (Pair a b)

instance (Arbitrary a
         , Arbitrary b) =>
         Arbitrary (Pair a b) where
  arbitrary = pairGen

pairGenIntString :: Gen (Pair Int String)
pairGenIntString = pairGen

main :: IO ()
main = sample pairGenIntString

