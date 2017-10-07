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

    it "15 myMult 5 is 75" $ do
      15 `myMult` 5 `shouldBe` (75 :: Integer)
    it "1 myMult 432434 is 432434" $ do
      1 `myMult` 432434 `shouldBe` (432434 :: Integer)
    it "0 myMult 10 is 0" $ do
      0 `myMult` 10 `shouldBe` (0 :: Integer)



-- A blast from the past, something from recursion in ch08!
dividedBy :: Integer -> Integer -> (Integer, Integer)
dividedBy num dem = go num dem 0
  where go n d count
          | n < d     = (count, n)
          | otherwise = go (n - d) d (count + 1)


-- More of a blast from the past, something else from recursion in ch08!
-- multiplication by sum
myMult :: (Integral a) => a -> a -> a
myMult 1 y = y
myMult x 1 = x
myMult x y = myMult x (y - 1) + x
