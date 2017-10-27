module Idempotence ( run ) where

import AsPatterns
import Test.QuickCheck
import Data.List (sort)

run :: IO ()
run = do
  quickCheck $ (f :: String -> Bool)
  quickCheck $ (f' :: [Integer] -> Bool)
  quickCheck $ (f' :: [Int] -> Bool)
  quickCheck $ (f' :: [String] -> Bool)

twice f = f . f
fourTimes = twice . twice

f x = (capitalizeWord x == twice capitalizeWord x)
      &&
      (capitalizeWord x == fourTimes capitalizeWord x)

f' x = (sort x == twice sort x)
       &&
       (sort x == fourTimes sort x)
