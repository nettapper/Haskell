module Addition where

import Test.Hspec

sayHello :: IO ()
sayHello = putStrLn "Hello!"

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      (1 + 1) > (1 :: Integer) `shouldBe` True
    it "2 + 2 should be 4" $ do
      (2 + 2) `shouldBe` (4 :: Integer)

    it "15 dividedBy 3 is 5" $ do
      dividedBy 15 3 `shouldBe` (5, 0)
    it "22 dividedBy 5 is 4 remainder 2" $ do
      dividedBy 22 5 `shouldBe` (4, 2)



-- A blast from the past, something from recursion in ch08!
dividedBy :: Integer -> Integer -> (Integer, Integer)
dividedBy num dem = go num dem 0
  where go n d count
          | n < d     = (count, n)
          | otherwise = go (n - d) d (count + 1)


