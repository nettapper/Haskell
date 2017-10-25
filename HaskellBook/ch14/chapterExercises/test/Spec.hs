import WordNumber (digitToWord, digits, wordNumber)
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "digitToWord" $ do
    it "should return zero for 0" $ do
      digitToWord 0 `shouldBe` "zero"

    it "should return one for 1" $ do
      digitToWord 1 `shouldBe` "one"

  describe "digits" $ do
    it "should return [1] for 1" $ do
      digits 1 `shouldBe` [1]

    it "should return [1,0,0] for 100" $ do
      digits 100 `shouldBe` [1,0,0]

  describe "wordNumber" $ do
    it "should return one-zero-zero given 100" $ do
      wordNumber 100 `shouldBe` "one-zero-zero"

    it "should return nine-zero-zero-one given 9001" $ do
      wordNumber 9001 `shouldBe` "nine-zero-zero-one"
