module ThyFearfulSymmetry where

-- *Main> myWords "all i wanna do is have some fun"
-- ["all","i","wanna","do","is","have","some","fun"]
-- Use takeWhile and dropWhile (use spaces as the delimiter)

myWords :: String -> [String]
myWords s = myWordsHelper s []
  where myWordsHelper :: String -> [String] -> [String]
        myWordsHelper x ys
              | null x = ys
              | otherwise = myWordsHelper (dropWordAndSpace x) (ys ++ [takeWhile (/= ' ') x])

dropWordAndSpace :: String -> String
dropWordAndSpace l = dropWhile (== ' ') (dropWhile (/= ' ') l)


-- myLines :: String -> [String]
-- myLines x = myLinesHelper x []

-- myLinesHelper :: String -> [String] -> [String]
-- myLinesHelper [] xs = xs
-- myLinesHelper x xs = myLinesHelper (dropLine x) (xs ++ [takeLine x])
--   where dropLine = dropWhile (== '\n') . dropWhile (/= '\n')
--         takeLine = takeWhile (/= '\n')

myWords' :: String -> [String]
myWords' x = splitter ' ' x []

myLines' :: String -> [String]
myLines' x = splitter '\n' x []


splitter :: Char -> String -> [String] -> [String]
splitter _ [] xs = xs
splitter delimiter x xs = splitter delimiter (dropLine x) (xs ++ [takeLine x])
  where dropLine = dropWhile (== delimiter) . dropWhile (/= delimiter)
        takeLine = takeWhile (/= delimiter)
