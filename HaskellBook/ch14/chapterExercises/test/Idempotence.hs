module Idempotence ( run ) where

import AsPatterns
import Test.QuickCheck

run :: IO ()
run = do
  quickCheck $ (f :: String -> Bool)

twice f = f . f
fourTimes = twice . twice

f x = (capitalizeWord x == twice capitalizeWord x)
      &&
      (capitalizeWord x == fourTimes capitalizeWord x)
