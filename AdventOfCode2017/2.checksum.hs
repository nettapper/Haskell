#!/usr/bin/env stack
-- stack --resolver lts-9.6 script
--
import Test.Hspec
import System.IO (isEOF)

main :: IO ()
main = do
  lines <- myLoop []
  let spreadSheet = (map ((map read ) . mySplit (== ' ')) lines) :: [[Integer]]
  let mins = map minimum spreadSheet
  let maxs = map maximum spreadSheet
  let sums = zipWith (-) maxs mins
  let checksum = sum sums
  putStrLn $ show $ checksum


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
  where go _ [] _ list = list
        go pred (s:ss) curr list = if pred s
                                      then go pred ss [] (list ++ [curr])
                                      else go pred ss (curr ++ [s]) list
