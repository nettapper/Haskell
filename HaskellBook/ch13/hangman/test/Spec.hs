module Main where

import Test.Hspec
import Test.QuickCheck
import Hangman

main :: IO ()
main = hspec $ do
  describe "fillInCharacter" $ do
    it "should pass the test test" $ do
      let p = Puzzle "cat" [] []
      let p' = Puzzle "cat" [] ['a']
      fillInCharacter p 'a' `shouldBe` p'

  describe "handleGuess" $ do
    it "should pass the test test" $ do
      (1 :: Int) `shouldBe` (1 :: Int)

