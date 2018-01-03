#!/usr/bin/env stack
-- stack --resolver lts-9.6 script
import Test.Hspec

captcha :: String -> Integer
captcha [] = 0
captcha ds@(d:_) = seqSum $ ds ++ [d]

seqSum :: String -> Integer
seqSum [] = 0
seqSum [a] = 0
seqSum (a:b:others) = if a == b
                         then (read (b:"") :: Integer) + seqSum (b:others)
                         else seqSum (b:others)

main :: IO ()
main = do
  l <- getLine
  putStrLn $ "the captcha of " ++ l ++ " is: " ++ show (captcha l)
  putStrLn "------"
  hspec $ do
    describe "captcha" $ do
      it "1122 should be 3" $ do
        captcha "1122" `shouldBe` 3

      it "1111 should be 4" $ do
        captcha "1111" `shouldBe` 4

      it "1234 should be 0" $ do
        captcha "1234" `shouldBe` 0

      it "91212129 should be 3" $ do
        captcha "91212129" `shouldBe` 9
