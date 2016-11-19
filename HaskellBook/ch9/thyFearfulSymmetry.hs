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
