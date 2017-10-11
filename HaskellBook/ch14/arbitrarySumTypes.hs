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

sumGenFirstPlz :: ( Arbitrary a
                  , Arbitrary b
                  ) =>
                  Gen (Sum a b)
sumGenFirstPlz = do
  a <- arbitrary
  b <- arbitrary
  frequency [ (10, return $ First a)
            , (1, return $ Second b)]

main :: IO ()
main = do
  sample sumGenCharInt
  putStrLn " --- "
  sample (sumGenFirstPlz :: Gen (Sum String Int))
