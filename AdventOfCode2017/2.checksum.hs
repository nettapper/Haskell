#!/usr/bin/env stack
-- stack --resolver lts-9.6 script
--
import Test.Hspec
import System.IO (isEOF)

main :: IO ()
main = do
  lines <- myLoop []
  let spreadSheet = (map ((map read ) . mySplit (== ' ')) lines) :: [[Integer]]
  putStrLn $ show $ calcChecksum spreadSheet
  putStrLn "---"
  runHspecTests

calcChecksum :: [[Integer]] -> Integer
calcChecksum spreadSheet = do
  let mins = map minimum spreadSheet
  let maxs = map maximum spreadSheet
  let sums = zipWith (-) maxs mins
  let checksum = sum sums
  checksum

myLoop :: [String] -> IO [String]
myLoop lines = do
  done <- isEOF
  if done
     then return lines
     else do
       l <- getLine
       myLoop (lines ++ [l])

mySplit :: (Char -> Bool) -> String -> [String]
mySplit pred str = go pred str [] []
  where go _ [] [] list = list
        go _ [] curr list = list ++ [curr]
        go pred (s:ss) curr list = if pred s
                                      then go pred ss [] (list ++ [curr])
                                      else go pred ss (curr ++ [s]) list

runHspecTests :: IO ()
runHspecTests =
  hspec $ do
    describe "calcChecksum" $ do
      it "example given should be 18" $ do
        let l = [[5,1,9,5]
                ,[7,5,3]
                ,[2,4,6,8]]
        calcChecksum l `shouldBe` 18

    describe "mySplit" $ do
      it "should return a empty for an empty" $ do
        mySplit (== ' ') [] `shouldBe` []
      it "should 4 elems given \"5 1 9 5\"" $ do
        let l = mySplit (== ' ') "5 1 9 5"
        length l `shouldBe` 4
