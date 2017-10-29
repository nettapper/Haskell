module Main where

import Test.Hspec
import Hangman

main :: IO ()
main = hspec $ do
  describe "fillInCharacter" $ do
    it "should add a character to an empty puzzle" $ do
      let p = Puzzle "cat" [] []
      let p' = Puzzle "cat" [] ['b']
      fillInCharacter p 'b' `shouldBe` p'

    it "should track all guesses, even when its the same character" $ do
      let p = Puzzle "cat" [] []
      let p' = Puzzle "cat" [] "bb"
      let pb = (fillInCharacter p 'b')
      fillInCharacter pb 'b' `shouldBe` p'

  describe "handleGuess" $ do
    it "should pass the test test" $ do
      let p = Puzzle "cat" [] []
      p' <- handleGuess p 'a'
      p' `shouldBe` Puzzle "cat" [] ['a']

