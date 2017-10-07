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

