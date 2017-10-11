module Main where

import Test.QuickCheck
import Test.QuickCheck.Gen (oneof)

data Sum a b =
    First a
  | Second b
  deriving (Show, Eq)

sumGenEqual :: ( Arbitrary a
               , Arbitrary b
               ) =>
               Gen (Sum a b)
sumGenEqual = do
  a <- arbitrary
  b <- arbitrary
  oneof [ return $ First a
        , return $ Second b
        ]

sumGenCharInt :: Gen (Sum Char Int)
sumGenCharInt = sumGenEqual

main :: IO ()
main = sample sumGenCharInt
